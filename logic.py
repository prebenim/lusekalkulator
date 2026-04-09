import numpy as np
from scipy.stats import nbinom, binom

# Model Constants
PARAMETER_HUNNLUS_SANN = np.array([-3.22963, 0.89694])
THETA_HELE = 1.997114
THETA_MERD = 1.693027

PARAMETERE_MERD_COUNT = np.array([-0.11920679,  0.64625410,  0.09369016,  0.19196262,  0.35336360,  0.19125739, -0.11604661])
PARAMETERE_MERD_ZERO = np.array([0.4520806, -1.3697959, -0.3342084, -0.6574109, -0.2297210,  0.3067283])

PARAMETERE_HELE_COUNT = np.array([0.62806465,  0.24380189,  0.05246029,  0.53724600,  0.14725647,  0.29432465,  0.15317690, -0.08139975])
PARAMETERE_HELE_ZERO = np.array([-1.3095828, -0.7798661, -1.0818692, -0.2903852, -0.5847494, -0.1541673,  0.2945743])

LOG_ADD = 0.1

def count_hele(merder_m1, lok_m1, vekt_merd, l_m1, re_fis, ln_smp):
    # Matching R's countHele exactly
    # Note: R uses ParametereMerdCount[3] for the squared term in countHele.
    co = (PARAMETERE_HELE_COUNT[0] +
          PARAMETERE_HELE_COUNT[1] * np.log(merder_m1 + LOG_ADD) +
          PARAMETERE_MERD_COUNT[2] * (np.log(merder_m1 + LOG_ADD)**2) +
          PARAMETERE_HELE_COUNT[3] * np.log(lok_m1 + LOG_ADD) +
          PARAMETERE_HELE_COUNT[4] * vekt_merd +
          PARAMETERE_HELE_COUNT[5] * l_m1 +
          PARAMETERE_HELE_COUNT[6] * ln_smp +
          PARAMETERE_HELE_COUNT[7] * re_fis)
    return co

def count_merd(merder_m1, vekt_merd, l_m1, re_fis, ln_smp):
    co = (PARAMETERE_MERD_COUNT[0] +
          PARAMETERE_MERD_COUNT[1] * np.log(merder_m1 + LOG_ADD) +
          PARAMETERE_MERD_COUNT[2] * (np.log(merder_m1 + LOG_ADD)**2) +
          PARAMETERE_MERD_COUNT[3] * vekt_merd +
          PARAMETERE_MERD_COUNT[4] * l_m1 +
          PARAMETERE_MERD_COUNT[5] * ln_smp +
          PARAMETERE_MERD_COUNT[6] * re_fis)
    return co

def zero_hele(merder_m1, lok_m1, vekt_merd, l_m1, re_fis, ln_smp):
    ze = (PARAMETERE_HELE_ZERO[0] +
          PARAMETERE_HELE_ZERO[1] * np.log(merder_m1 + LOG_ADD) +
          PARAMETERE_HELE_ZERO[2] * np.log(lok_m1 + LOG_ADD) +
          PARAMETERE_HELE_ZERO[3] * vekt_merd +
          PARAMETERE_HELE_ZERO[4] * l_m1 +
          PARAMETERE_HELE_ZERO[5] * ln_smp +
          PARAMETERE_HELE_ZERO[6] * re_fis)
    return ze

def zero_merd(merder_m1, vekt_merd, l_m1, re_fis, ln_smp):
    ze = (PARAMETERE_MERD_ZERO[0] +
          PARAMETERE_MERD_ZERO[1] * np.log(merder_m1 + LOG_ADD) +
          PARAMETERE_MERD_ZERO[2] * vekt_merd +
          PARAMETERE_MERD_ZERO[3] * l_m1 +
          PARAMETERE_MERD_ZERO[4] * ln_smp +
          PARAMETERE_MERD_ZERO[5] * re_fis)
    return ze

def trekk(p0, p_count, theta):
    if binom.rvs(1, p0) == 1:
        return 0
    else:
        mu = p_count
        n = theta
        p = n / (n + mu)
        return nbinom.rvs(n, p)

def run_simulation(merder_m1, vekt_merd, re_fis, l_m1, ln_smp, whole_site=True):
    n_merds = len(merder_m1)
    # R uses thetaHele for everything in plotLok simulation calls
    theta = THETA_HELE

    current_merder = np.array(merder_m1, dtype=float)
    vekt_merd = np.array(vekt_merd, dtype=float)
    re_fis = np.array(re_fis, dtype=float)
    l_m1 = np.array(l_m1, dtype=float)

    # all_weeks_results will store week 0, 1, 2, 3
    # Week 0 is just the input means
    all_weeks_results = [{'means': current_merder.tolist()}]

    for week in range(1, 4):
        lok_m1 = np.mean(current_merder)
        if whole_site:
            y_count = count_hele(current_merder, lok_m1, vekt_merd, l_m1, re_fis, ln_smp)
            y_zero = zero_hele(current_merder, lok_m1, vekt_merd, l_m1, re_fis, ln_smp)
        else:
            y_count = count_merd(current_merder, vekt_merd, l_m1, re_fis, ln_smp)
            y_zero = zero_merd(current_merder, vekt_merd, l_m1, re_fis, ln_smp)

        p_count = np.exp(y_count)
        p_zero = np.exp(y_zero) / (1 + np.exp(y_zero))

        week_means = []
        week_q25 = []
        week_q75 = []

        for i in range(n_merds):
            obs = np.array([trekk(p_zero[i], p_count[i], theta) for _ in range(1000)])
            week_means.append(np.mean(obs) / 30)
            week_q25.append(np.quantile(obs, 0.25) / 30)
            week_q75.append(np.quantile(obs, 0.75) / 30)

        current_merder = np.array(week_means)
        all_weeks_results.append({
            'means': week_means,
            'q25': week_q25,
            'q75': week_q75
        })

    return all_weeks_results

def calculate_hunnlus_prob(mean_mobile):
    logits = PARAMETER_HUNNLUS_SANN[0] + PARAMETER_HUNNLUS_SANN[1] * mean_mobile
    prob = 1 / (1 + np.exp(-logits))
    return 100 * prob
