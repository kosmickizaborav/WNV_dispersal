# ml_hurdle.py
# Robust ML density + MLE fits: positive-only and hurdle (point mass at zero).
# Dependencies: numpy, scipy
from __future__ import annotations
import numpy as np
from scipy.special import gamma as sc_gamma, expit, logit
from scipy.stats import levy_stable
from scipy.optimize import minimize
from typing import Dict, Any, Sequence, Tuple

VERY_SMALL = 1e-300
PENALTY = 1e12
EPS = 1e-12

def ml_distr(xi: Sequence[float], beta: float) -> np.ndarray:
    """
    ML density as in the Python snippet you gave.
    xi: positive numeric sequence
    beta: stability parameter (we require 0 < beta < 1 for real-valued scale)
    Returns: density values (very small positive numbers if numerically invalid)
    """
    xi = np.asarray(xi, dtype=float)
    if xi.size == 0:
        return xi.copy()
    if np.any(xi <= 0):
        raise ValueError("xi must be > 0 for ml_distr")
    if not np.isfinite(beta) or not (0.0 < beta < 1.0):
        return np.full_like(xi, VERY_SMALL, dtype=float)

    # keep beta away from exact boundaries
    beta = float(np.clip(beta, 1e-12, 1.0 - 1e-12))
    b_m = 1.0 / beta
    gg = sc_gamma(1.0 + beta)

    # compute log(xx) for numerical stability
    log_xx = b_m * np.log(gg) - b_m * np.log(xi)
    # if overflow/underflow risk -> penalize by returning tiny values (avoid exceptions)
    if np.any(log_xx > 700) or np.any(log_xx < -700):
        return np.full_like(xi, VERY_SMALL, dtype=float)
    xx = np.exp(log_xx)

    cosval = np.cos(np.pi * beta / 2.0)
    if cosval <= 0:
        return np.full_like(xi, VERY_SMALL, dtype=float)
    scale = cosval ** (1.0 / beta)

    ll = levy_stable.pdf(xx, beta, 1.0, loc=0.0, scale=scale)
    # defensive: replace bad evaluations
    ll = np.where(np.isfinite(ll) & (ll > 0), ll, VERY_SMALL)

    pdf = (gg ** b_m) / (beta * (xi ** (1.0 + b_m))) * ll
    pdf = np.where(np.isfinite(pdf) & (pdf > 0), pdf, VERY_SMALL)
    return pdf

# ---------- Positive-only MLE (fits beta using xi>0) ----------
def neg_loglik_beta_scalar(beta: float, xi: np.ndarray) -> float:
    try:
        pdfs = ml_distr(xi, beta)
    except Exception:
        return PENALTY
    if np.any(~np.isfinite(pdfs)) or np.any(pdfs <= 0):
        return PENALTY
    return -np.sum(np.log(pdfs))

def fit_ml_beta(xi: Sequence[float],
                starts: Sequence[float] = (0.2, 0.5, 0.8),
                bounds: Tuple[float,float] = (1e-6, 1-1e-6),
                method: str = 'L-BFGS-B') -> Dict[str, Any]:
    """
    Fit beta by MLE on positive xi (no zeros).
    Returns dict with beta_hat, negloglik, success, message, optim_result.
    """
    xi = np.asarray(xi, dtype=float)
    if np.any(xi <= 0):
        raise ValueError("fit_ml_beta requires strictly positive xi (no zeros).")

    best = None
    best_val = np.inf
    lb, ub = bounds
    for s in starts:
        x0 = np.array([float(np.clip(s, lb + 1e-12, ub - 1e-12))])
        res = minimize(lambda p: neg_loglik_beta_scalar(p[0], xi),
                       x0=x0, bounds=[bounds], method=method)
        if res.fun < best_val:
            best_val = res.fun
            best = res

    if best is None:
        # fallback grid
        betas = np.linspace(lb, ub, 51)
        vals = [neg_loglik_beta_scalar(b, xi) for b in betas]
        idx = int(np.argmin(vals))
        return {'beta_hat': float(betas[idx]), 'negloglik': float(vals[idx]),
                'success': False, 'message': 'grid fallback', 'bootstrap': np.array([])}
    return {'beta_hat': float(best.x[0]), 'negloglik': float(best.fun),
            'success': bool(best.success), 'message': best.message, 'optim_result': best}

# ---------- HURDLE model (p0 at zero + ML for positives) ----------
def neg_loglik_hurdle(params_unconstrained: Sequence[float], xi: Sequence[float]) -> float:
    """
    params_unconstrained: length-2 array: [a, b] where p0 = expit(a), beta = expit(b)
    xi: full data (may contain zeros)
    Returns negative log-likelihood (large penalty for invalid parameter regions).
    """
    xi = np.asarray(xi, dtype=float)
    a, b = float(params_unconstrained[0]), float(params_unconstrained[1])
    p0 = expit(a)  # in (0,1)
    beta = expit(b)  # in (0,1)

    # keep beta away from boundary
    beta = float(np.clip(beta, 1e-12, 1.0 - 1e-12))

    # contributions
    zeros_mask = (xi == 0)
    n_zeros = int(np.sum(zeros_mask))
    positives = xi[~zeros_mask]

    # log-lik for zeros: sum log(p0)
    ll_zero = n_zeros * np.log(max(p0, VERY_SMALL))

    # log-lik for positives: sum log(1-p0) + sum log f_ml
    if positives.size == 0:
        ll_pos = 0.0
    else:
        pdfs = ml_distr(positives, beta)
        if np.any(~np.isfinite(pdfs)) or np.any(pdfs <= 0):
            return PENALTY
        ll_pos = positives.size * np.log(max(1.0 - p0, VERY_SMALL)) + np.sum(np.log(pdfs))

    nll = -(ll_zero + ll_pos)
    if not np.isfinite(nll):
        return PENALTY
    return float(nll)

def fit_ml_hurdle(xi: Sequence[float],
                  starts: Sequence[Tuple[float,float]] = ((0.1, 0.5), (0.5, 0.5), (0.2,0.8)),
                  method: str = 'BFGS') -> Dict[str, Any]:
    """
    Fit hurdle model (p0, beta) by MLE. xi can contain zeros.
    starts: list of (p0_init, beta_init) in (0,1) space.
    Returns estimated p0, beta, negloglik, success, message, and optim_result.
    """
    xi = np.asarray(xi, dtype=float)
    # basic checks
    if xi.size == 0:
        raise ValueError("No data provided.")
    if np.any(xi < 0):
        raise ValueError("xi must be >= 0 for hurdle model.")

    best = None
    best_val = np.inf
    for s in starts:
        p0_init = float(np.clip(s[0], 1e-6, 1-1e-6))
        beta_init = float(np.clip(s[1], 1e-6, 1-1e-6))
        # transform to unconstrained (logit)
        a0 = logit(p0_init)
        b0 = logit(beta_init)
        x0 = np.array([a0, b0], dtype=float)
        res = minimize(lambda par: neg_loglik_hurdle(par, xi),
                       x0=x0, method=method)
        if res.fun < best_val:
            best_val = res.fun
            best = res

    if best is None:
        return {'p0': np.nan, 'beta': np.nan, 'negloglik': np.nan, 'success': False, 'message': 'no fit'}
    a_hat, b_hat = float(best.x[0]), float(best.x[1])
    p0_hat = expit(a_hat)
    beta_hat = expit(b_hat)
    return {'p0': float(p0_hat), 'beta': float(beta_hat), 'negloglik': float(best.fun),
            'success': bool(getattr(best, 'success', False)), 'message': getattr(best, 'message', ''), 'optim_result': best}

# ---------- Bootstrap helpers ----------
def bootstrap_hurdle(xi: Sequence[float], n_boot: int = 200, starts=None, random_state=None) -> Dict[str, Any]:
    xi = np.asarray(xi, dtype=float)
    base = fit_ml_hurdle(xi, starts=starts if starts is not None else ((0.1,0.5),(0.5,0.5)))
    rng = np.random.default_rng(random_state)
    p0_samps = []
    beta_samps = []
    for _ in range(n_boot):
        s = rng.choice(xi, size=xi.size, replace=True)
        try:
            r = fit_ml_hurdle(s, starts=starts if starts is not None else ((0.1,0.5),(0.5,0.5)))
            p0_samps.append(r['p0'])
            beta_samps.append(r['beta'])
        except Exception:
            # record NaN and continue
            p0_samps.append(np.nan)
            beta_samps.append(np.nan)
    p0_samps = np.array(p0_samps)
    beta_samps = np.array(beta_samps)
    res = {
        'p0_hat': base['p0'],
        'beta_hat': base['beta'],
        'p0_boot': p0_samps,
        'beta_boot': beta_samps,
        'p0_ci': (np.nanpercentile(p0_samps[~np.isnan(p0_samps)], 2.5) if np.any(~np.isnan(p0_samps)) else np.nan,
                  np.nanpercentile(p0_samps[~np.isnan(p0_samps)], 97.5) if np.any(~np.isnan(p0_samps)) else np.nan),
        'beta_ci': (np.nanpercentile(beta_samps[~np.isnan(beta_samps)], 2.5) if np.any(~np.isnan(beta_samps)) else np.nan,
                    np.nanpercentile(beta_samps[~np.isnan(beta_samps)], 97.5) if np.any(~np.isnan(beta_samps)) else np.nan)
    }
    return res