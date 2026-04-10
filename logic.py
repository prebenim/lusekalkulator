import numpy as np
from scipy.stats import poisson, norm

# Stige et al. (2024) Model Coefficients
COEFFS = {
    'aS0': 0.0093,
    'aO0': 0.024,
    'aA0': 0.0065,
    'aSL': 1.1,
    's1': 0.49,
    's2': 2.3,
    's3': 0.88,
    's4': 0.61,
    'a10': -2.4,
    'a11': 0.37,
    'a20': -2.1,
    'a21': 0.037,
    'aS_NFISH': -0.0024,
    'aO_NFISH': -0.00082,
    'aO_WFISH': 0.078,
    'aA_WFISH': 0.11,
    'aA_WRASSE': -0.20,
    'wSF': 0.88,
    'wOF': 0.63,
    'wAF': 0.54,
    'sigma1': 0.53,
    'sigma2S': 1.2,
    'sigma2O': 0.42,
    'sigma2A': 0.35
}

# Centering Constants
CENTER = {
    'TEMP': 9.0,
    'WFISH': 2.2,
    'NFISH': 130.0
}

def calculate_dT(temp):
    """Calculates the temperature-dependent development time (dT) in weeks."""
    # dT = 0.5 * (1 / (0.000677*T^2 + 0.010294*T + 0.005729))^0.354753 +
    #      0.5 * (1 / (0.000485*T^2 + 0.008667*T + 0.003750))^0.152008
    term1_den = 0.000677 * (temp**2) + 0.010294 * temp + 0.005729
    term2_den = 0.000485 * (temp**2) + 0.008667 * temp + 0.003750
    dT = 0.5 * (1.0 / term1_den)**0.354753 + 0.5 * (1.0 / term2_den)**0.152008
    return dT

def get_d1(temp):
    ctemp = temp - CENTER['TEMP']
    logit_d1 = COEFFS['a10'] + COEFFS['a11'] * ctemp
    return np.exp(logit_d1) / (1 + np.exp(logit_d1))

def get_d2(temp):
    ctemp = temp - CENTER['TEMP']
    logit_d2 = COEFFS['a20'] + COEFFS['a21'] * ctemp
    return np.exp(logit_d2) / (1 + np.exp(logit_d2))

def calculate_mu_s(S_hat, n_fish, infestation_pressure, d1):
    cnfish = n_fish - CENTER['NFISH']
    # Equation 16
    term1 = COEFFS['aS0'] + (1 - d1) * COEFFS['s1'] * S_hat + COEFFS['aSL'] * infestation_pressure
    term2 = np.exp(COEFFS['aS_NFISH'] * cnfish)
    term3 = np.exp(0.5 * (COEFFS['sigma1']**2 + COEFFS['sigma2S']**2))
    return term1 * term2 * term3

def calculate_mu_o(S_hat, O_hat, n_fish, w_fish, d1, d2):
    cnfish = n_fish - CENTER['NFISH']
    cwfish = w_fish - CENTER['WFISH']
    # Equation 17
    term1 = COEFFS['aO0'] + (1 - d2) * COEFFS['s3'] * O_hat + d1 * COEFFS['s2'] * S_hat
    term2 = np.exp(COEFFS['aO_NFISH'] * cnfish + COEFFS['aO_WFISH'] * cwfish)
    term3 = np.exp(0.5 * (COEFFS['sigma1']**2 + COEFFS['sigma2O']**2))
    return term1 * term2 * term3

def calculate_mu_a(O_hat, A_hat, w_fish, wrasse, d2):
    cwfish = w_fish - CENTER['WFISH']
    # Equation 18
    term1 = COEFFS['aA0'] + COEFFS['s4'] * A_hat + d2 * (0.5 * COEFFS['s3'] + 0.5 * COEFFS['s4']) * O_hat
    term2 = np.exp(COEFFS['aA_WFISH'] * cwfish + COEFFS['aA_WRASSE'] * wrasse)
    term3 = np.exp(0.5 * (COEFFS['sigma1']**2 + COEFFS['sigma2A']**2))
    return term1 * term2 * term3

def run_simulation(cages_data, temp, infestation_pressure_weeks):
    """
    cages_data: list of dicts with keys 'S', 'O', 'A', 'NFISH', 'WFISH', 'WRASSE'
    temp: site-wide temperature
    infestation_pressure_weeks: list of LAGGED infestation pressures for next 3 weeks [P_t+1-dT, P_t+2-dT, P_t+3-dT]
    """
    n_cages = len(cages_data)
    results = []

    current_state = []
    for cage in cages_data:
        current_state.append({
            'S': cage['S'],
            'O': cage['O'],
            'A': cage['A'],
            'NFISH': cage['NFISH'],
            'WFISH': cage['WFISH'],
            'WRASSE': cage['WRASSE']
        })

    results.append({'cages': [{'mu_a': c['A'], 'prob_exceed': 0.0, 'q25_a': c['A'], 'q75_a': c['A']} for c in current_state]})

    n_sims = 1000
    for week in range(1, 4):
        S_vals = np.array([c['S'] for c in current_state])
        O_vals = np.array([c['O'] for c in current_state])
        A_vals = np.array([c['A'] for c in current_state])

        S_farm_mean = np.mean(S_vals)
        O_farm_mean = np.mean(O_vals)
        A_farm_mean = np.mean(A_vals)

        S_hat = (1 - COEFFS['wSF']) * S_vals + COEFFS['wSF'] * S_farm_mean
        O_hat = (1 - COEFFS['wOF']) * O_vals + COEFFS['wOF'] * O_farm_mean
        A_hat = (1 - COEFFS['wAF']) * A_vals + COEFFS['wAF'] * A_farm_mean

        d1 = get_d1(temp)
        d2 = get_d2(temp)
        P = infestation_pressure_weeks[week-1]

        new_state = []
        cage_results = []

        # Farm-week random effect
        eps1 = np.random.normal(0, COEFFS['sigma1'], n_sims)

        for i in range(n_cages):
            mu_s = calculate_mu_s(S_hat[i], current_state[i]['NFISH'], P, d1)
            mu_o = calculate_mu_o(S_hat[i], O_hat[i], current_state[i]['NFISH'], current_state[i]['WFISH'], d1, d2)
            mu_a = calculate_mu_a(O_hat[i], A_hat[i], current_state[i]['WFISH'], current_state[i]['WRASSE'], d2)

            eps2_a = np.random.normal(0, COEFFS['sigma2A'], n_sims)
            lambda_a = mu_a / np.exp(0.5 * (COEFFS['sigma1']**2 + COEFFS['sigma2A']**2)) * np.exp(eps1 + eps2_a)

            prob_exceed = np.mean(lambda_a > 0.5) * 100

            new_state.append({
                'S': mu_s,
                'O': mu_o,
                'A': mu_a,
                'NFISH': current_state[i]['NFISH'],
                'WFISH': current_state[i]['WFISH'],
                'WRASSE': current_state[i]['WRASSE']
            })

            cage_results.append({
                'mu_a': mu_a,
                'prob_exceed': prob_exceed,
                'q25_a': np.quantile(lambda_a, 0.25),
                'q75_a': np.quantile(lambda_a, 0.75)
            })

        current_state = new_state
        results.append({
            'cages': cage_results
        })

    return results
