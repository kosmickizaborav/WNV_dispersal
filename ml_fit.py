# ml_fit.py
# Implements the ml_distr you posted, a robust MLE fit for beta in (0,1),
# optional bootstrap, and a small API for calling from R via reticulate.
import numpy as np
from scipy.special import gamma as sc_gamma
from scipy.stats import levy_stable
from scipy.optimize import minimize

VERY_SMALL = 1e-300
PENALTY = 1e12

def ml_distr(xi, beta):
    """
    ml_distr(xi, beta)
    xi : array-like of positive values
    beta: scalar (should be in (0,1) for the formula to make sense)
    returns: numpy array of density values (very small positive for underflow)
    """
    xi = np.asarray(xi, dtype=float)
    if np.any(xi <= 0):
        raise ValueError("xi must be > 0 for ml_distr")
    if not np.isfinite(beta) or not (0.0 < beta < 1.0):
        # return tiny densities (so log-lik becomes huge) instead of error
        return np.full_like(xi, VERY_SMALL, dtype=float)

    # numeric safeguard: keep beta away from the boundary
    beta = float(np.clip(beta, 1e-12, 1.0 - 1e-12))

    b_m = 1.0 / beta
    gg = sc_gamma(1.0 + beta)                # gamma(1+beta)
    # compute xx = (gamma(1+beta)^b_m) / xi^b_m  using logs for stability
    log_xx = b_m * np.log(gg) - b_m * np.log(xi)
    # if values are astronomically large or tiny, return tiny densities
    if np.any(log_xx > 700) or np.any(log_xx < -700):
        # avoid overflow in exp; return tiny values to penalize this beta
        return np.full_like(xi, VERY_SMALL, dtype=float)
    xx = np.exp(log_xx)

    cosval = np.cos(np.pi * beta / 2.0)
    if cosval <= 0:
        return np.full_like(xi, VERY_SMALL, dtype=float)
    scale = cosval ** (1.0 / beta)

    # Evaluate stable pdf at xx. In scipy levy_stable.pdf(x, alpha, beta)
    # Here alpha = beta (stability), skewness = 1 (completely skewed)
    ll = levy_stable.pdf(xx, beta, 1.0, loc=0.0, scale=scale)

    # replace non-finite or <=0 with very small number to avoid log(0)
    ll = np.where(np.isfinite(ll) & (ll > 0), ll, VERY_SMALL)

    pdf = (gg ** b_m) / (beta * (xi ** (1.0 + b_m))) * ll
    # guard against underflow/NaN
    pdf = np.where(np.isfinite(pdf) & (pdf > 0), pdf, VERY_SMALL)
    return pdf

def neg_loglik_scalar(beta, xi):
    """Negative log-likelihood for scalar beta. Returns large penalty on invalid."""
    try:
        pdfs = ml_distr(xi, beta)
    except Exception:
        return PENALTY
    if np.any(~np.isfinite(pdfs)) or np.any(pdfs <= 0):
        return PENALTY
    return -np.sum(np.log(pdfs))

def fit_ml_beta(xi, starts=(0.2, 0.5, 0.8), bounds=(1e-6, 1-1e-6), method='L-BFGS-B', verbose=False):
    """
    Fit beta by MLE. xi: 1-d array-like of positive values (no zeros).
    starts: iterable of initial beta guesses
    returns: dict with best beta, optimization result, and negative loglik
    """
    xi = np.asarray(xi, dtype=float)
    if np.any(xi <= 0):
        raise ValueError("Input xi must be > 0. If you have zeros, handle them (hurdle/censoring) before calling this function.")

    best = None
    best_val = np.inf
    lb, ub = bounds
    for s in starts:
        x0 = np.array([float(np.clip(s, lb + 1e-12, ub - 1e-12))])
        res = minimize(lambda p: neg_loglik_scalar(p[0], xi),
                       x0=x0, bounds=[bounds], method=method)
        if res.success and res.fun < best_val:
            best_val = res.fun
            best = res

    # if no success, pick the best even if not success (still may be informative)
    if best is None:
        # fallback: try grid search for reasonable beta
        betas = np.linspace(lb, ub, 50)
        vals = [neg_loglik_scalar(b, xi) for b in betas]
        idx = int(np.argmin(vals))
        return {
            'beta_hat': float(betas[idx]),
            'negloglik': float(vals[idx]),
            'success': False,
            'message': 'No optimization succeeded; returned best from grid search.'
        }

    return {
        'beta_hat': float(best.x[0]),
        'negloglik': float(best.fun),
        'success': bool(best.success),
        'message': best.message,
        'optim_result': best
    }

def fit_ml_beta_bootstrap(xi, n_boot=200, starts=(0.2,0.5,0.8), bounds=(1e-6,1-1e-6), random_state=None):
    """
    Fit beta by MLE and compute bootstrap CI.
    Returns dict with beta_hat, negloglik, bootstrap samples and CI (2.5/97.5).
    """
    xi = np.asarray(xi, dtype=float)
    base = fit_ml_beta(xi, starts=starts, bounds=bounds)
    beta_hat = base['beta_hat']
    rng = np.random.default_rng(random_state)
    boots = []
    for _ in range(n_boot):
        sample = rng.choice(xi, size=xi.size, replace=True)
        try:
            r = fit_ml_beta(sample, starts=starts, bounds=bounds)
            boots.append(r['beta_hat'])
        except Exception:
            boots.append(np.nan)
    boots = np.array(boots)
    boots = boots[np.isfinite(boots)]
    ci = (np.percentile(boots, 2.5) if boots.size else np.nan,
          np.percentile(boots, 97.5) if boots.size else np.nan)
    return {
        'beta_hat': beta_hat,
        'negloglik': base['negloglik'],
        'success': base['success'],
        'message': base.get('message',''),
        'bootstrap': boots,
        'ci_95': ci
    }

# small helper that R can call more simply
def fit_ml_from_list(xi_list, do_bootstrap=False, n_boot=200, starts=(0.2,0.5,0.8)):
    xi = np.asarray(xi_list, dtype=float)
    # remove NAs
    xi = xi[np.isfinite(xi)]
    # check zeros: if any zeros exist, raise informative error (user should handle zeros)
    if np.any(xi <= 0):
        raise ValueError("Data contains zeros or non-positive values. For this model you must either remove zeros or use a hurdle/censoring approach.")
    if do_bootstrap:
        return fit_ml_beta_bootstrap(xi, n_boot=n_boot, starts=starts)
    else:
        return fit_ml_beta(xi, starts=starts)