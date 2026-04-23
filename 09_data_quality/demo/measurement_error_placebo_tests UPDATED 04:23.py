

## Measurement error occurs when a variable is observed with noise, so the measured value deviates from the true underlying value. If the error is in a predictor that is not a confounder, it typically causes attenuation bias—shrinking its estimated coefficient toward zero without biasing the treatment effect. But if the error is in a confounder, the model only partially controls for it, leaving residual confounding; this can bias the estimated treatment effect because some of the confounder’s influence is mistakenly attributed to the treatment.

import os
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt


# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------
np.random.seed(123)

os.makedirs("outputs", exist_ok=True)
os.makedirs("figures", exist_ok=True)


# -----------------------------------------------------------------------------
# Helper functions
# -----------------------------------------------------------------------------
def ols_coef(X, y):
    coef, _, _, _ = np.linalg.lstsq(X, y, rcond=None)
    return coef


def simulate_base_data(n=5000, tau=1.0, beta=1.0, seed=123):
    rng = np.random.default_rng(seed)

    x_true = rng.normal(0.0, 1.0, n)

    logit_p = 1.0 * x_true
    p = 1.0 / (1.0 + np.exp(-logit_p))
    d = rng.binomial(1, p, n)

    eps_y = rng.normal(0.0, 1.0, n)
    y = tau * d + beta * x_true + eps_y

    eps_pl = rng.normal(0.0, 1.0, n)
    y_placebo = 0.0 * d + beta * x_true + eps_pl

    return {
        "x_true": x_true,
        "d": d,
        "y": y,
        "y_placebo": y_placebo,
        "tau_true": tau,
        "beta_true": beta,
        "n": n,
    }


def run_measurement_error_simulation(
    x_true,
    d,
    y,
    y_placebo,
    tau_true,
    beta_true,
    sigma_u_grid=(0.0, 0.2, 0.5, 1.0, 2.0),
    R=30,
    validation_share=0.20,
    seed=999,
):
    rng = np.random.default_rng(seed)
    n = len(x_true)
    ones = np.ones(n)

    validation_idx = rng.choice(np.arange(n), size=int(validation_share * n), replace=False)
    is_validation = np.zeros(n, dtype=bool)
    is_validation[validation_idx] = True

    rows = []

    for sigma_u in sigma_u_grid:
        tau_oracle_list = []
        tau_naive_list = []
        tau_cal_list = []
        tau_placebo_list = []

        beta_oracle_list = []
        beta_naive_list = []
        beta_cal_list = []

        for _ in range(R):
            u = rng.normal(0.0, sigma_u, n)
            x_obs = x_true + u

            # Oracle: y ~ d + x_true
            X_oracle = np.column_stack([ones, d, x_true])
            coef_oracle = ols_coef(X_oracle, y)
            tau_oracle_list.append(coef_oracle[1])
            beta_oracle_list.append(coef_oracle[2])

            # Naive: y ~ d + x_obs
            X_naive = np.column_stack([ones, d, x_obs])
            coef_naive = ols_coef(X_naive, y)
            tau_naive_list.append(coef_naive[1])
            beta_naive_list.append(coef_naive[2])

            # Regression calibration using validation sample
            X_cal_val = np.column_stack([np.ones(is_validation.sum()), x_obs[is_validation]])
            coef_cal = ols_coef(X_cal_val, x_true[is_validation])
            x_hat = coef_cal[0] + coef_cal[1] * x_obs

            X_calibrated = np.column_stack([ones, d, x_hat])
            coef_calibrated = ols_coef(X_calibrated, y)
            tau_cal_list.append(coef_calibrated[1])
            beta_cal_list.append(coef_calibrated[2])

            # Outcome placebo: y_placebo ~ d + x_obs
            X_placebo = np.column_stack([ones, d, x_obs])
            coef_placebo = ols_coef(X_placebo, y_placebo)
            tau_placebo_list.append(coef_placebo[1])

        rows.append(
            {
                "sigma_u": sigma_u,
                "tau_true": tau_true,
                "tau_oracle_mean": np.mean(tau_oracle_list),
                "tau_naive_mean": np.mean(tau_naive_list),
                "tau_cal_mean": np.mean(tau_cal_list),
                "tau_placebo_mean": np.mean(tau_placebo_list),
                "tau_oracle_q025": np.quantile(tau_oracle_list, 0.025),
                "tau_oracle_q975": np.quantile(tau_oracle_list, 0.975),
                "tau_naive_q025": np.quantile(tau_naive_list, 0.025),
                "tau_naive_q975": np.quantile(tau_naive_list, 0.975),
                "tau_cal_q025": np.quantile(tau_cal_list, 0.025),
                "tau_cal_q975": np.quantile(tau_cal_list, 0.975),
                "beta_true": beta_true,
                "beta_oracle_mean": np.mean(beta_oracle_list),
                "beta_naive_mean": np.mean(beta_naive_list),
                "beta_cal_mean": np.mean(beta_cal_list),
            }
        )

    return pd.DataFrame(rows)


def plot_measurement_error_results(results, tau_true, beta_true):
    # Tau plot
    plt.figure(figsize=(8, 5))
    plt.plot(results["sigma_u"], results["tau_oracle_mean"], marker="o", label="Oracle: y ~ d + x_true")
    plt.plot(results["sigma_u"], results["tau_naive_mean"], marker="o", label="Naive: y ~ d + x_obs")
    plt.plot(results["sigma_u"], results["tau_cal_mean"], marker="o", label="Calibration: y ~ d + x_hat")
    plt.plot(results["sigma_u"], results["tau_placebo_mean"], marker="o", label="Outcome placebo: y_pl ~ d + x_obs")
    plt.axhline(tau_true, linestyle="--", label="True tau")
    plt.title("Estimated treatment effect vs measurement error in confounder")
    plt.xlabel("Measurement error SD (sigma_u)")
    plt.ylabel("Estimated coefficient on d")
    plt.legend()
    plt.tight_layout()
    plt.savefig("figures/measurement_error_tau_vs_sigma.png", dpi=200)
    plt.close()

    # Beta plot
    plt.figure(figsize=(8, 5))
    plt.plot(results["sigma_u"], results["beta_oracle_mean"], marker="o", label="Oracle: coef on x_true")
    plt.plot(results["sigma_u"], results["beta_naive_mean"], marker="o", label="Naive: coef on x_obs")
    plt.plot(results["sigma_u"], results["beta_cal_mean"], marker="o", label="Calibration: coef on x_hat")
    plt.axhline(beta_true, linestyle="--", label="True beta")
    plt.title("Estimated confounder effect vs measurement error (attenuation)")
    plt.xlabel("Measurement error SD (sigma_u)")
    plt.ylabel("Estimated coefficient on confounder term")
    plt.legend()
    plt.tight_layout()
    plt.savefig("figures/measurement_error_beta_vs_sigma.png", dpi=200)
    plt.close()


def run_validation_share_analysis(
    x_true,
    d,
    y,
    sigma_u=1.0,
    validation_shares=(0.05, 0.20, 0.50),
    R=30,
    seed=2024,
):
    rng = np.random.default_rng(seed)
    n = len(x_true)
    ones = np.ones(n)

    rows = []

    for validation_share in validation_shares:
        tau_naive_list = []
        tau_cal_list = []

        validation_idx = rng.choice(np.arange(n), size=int(validation_share * n), replace=False)
        is_validation = np.zeros(n, dtype=bool)
        is_validation[validation_idx] = True

        for _ in range(R):
            u = rng.normal(0.0, sigma_u, n)
            x_obs = x_true + u

            # Naive
            X_naive = np.column_stack([ones, d, x_obs])
            coef_naive = ols_coef(X_naive, y)
            tau_naive_list.append(coef_naive[1])

            # Calibration
            X_cal_val = np.column_stack([np.ones(is_validation.sum()), x_obs[is_validation]])
            coef_cal = ols_coef(X_cal_val, x_true[is_validation])
            x_hat = coef_cal[0] + coef_cal[1] * x_obs

            X_calibrated = np.column_stack([ones, d, x_hat])
            coef_calibrated = ols_coef(X_calibrated, y)
            tau_cal_list.append(coef_calibrated[1])

        rows.append(
            {
                "validation_share": validation_share,
                "sigma_u": sigma_u,
                "tau_naive_mean": np.mean(tau_naive_list),
                "tau_cal_mean": np.mean(tau_cal_list),
            }
        )

    return pd.DataFrame(rows)


def run_outcome_placebo(x_true, d, y_placebo, sigma_u=1.0, seed=555):
    rng = np.random.default_rng(seed)
    n = len(x_true)
    ones = np.ones(n)

    u = rng.normal(0.0, sigma_u, n)
    x_obs = x_true + u

    X_placebo = np.column_stack([ones, d, x_obs])
    coef_placebo = ols_coef(X_placebo, y_placebo)

    return {
        "sigma_u": sigma_u,
        "placebo_coef_on_d": coef_placebo[1],
        "intercept": coef_placebo[0],
        "coef_on_x_obs": coef_placebo[2],
    }


def run_treatment_permutation_placebo(x_true, d, y, sigma_u=1.0, B=500, seed=777):
    rng = np.random.default_rng(seed)
    n = len(x_true)
    ones = np.ones(n)

    u = rng.normal(0.0, sigma_u, n)
    x_obs = x_true + u

    X_obs = np.column_stack([ones, d, x_obs])
    coef_obs = ols_coef(X_obs, y)
    tau_hat_obs = float(coef_obs[1])

    tau_perm = []
    for _ in range(B):
        d_perm = rng.permutation(d)
        X_b = np.column_stack([ones, d_perm, x_obs])
        coef_b = ols_coef(X_b, y)
        tau_perm.append(float(coef_b[1]))

    tau_perm = np.array(tau_perm)
    p_emp = (1.0 + np.sum(np.abs(tau_perm) >= np.abs(tau_hat_obs))) / (B + 1.0)

    # Save histogram
    plt.figure(figsize=(8, 5))
    plt.hist(tau_perm, bins=30, alpha=0.8)
    plt.axvline(tau_hat_obs, linestyle="--", linewidth=2, label=f"Observed tau_hat = {tau_hat_obs:.3f}")
    plt.axvline(-tau_hat_obs, linestyle="--", linewidth=1)
    plt.title(f"Treatment permutation placebo (sigma_u={sigma_u})\nEmpirical p-value = {p_emp:.3f}")
    plt.xlabel("Coefficient on permuted treatment")
    plt.ylabel("Count")
    plt.legend()
    plt.tight_layout()
    plt.savefig("figures/permutation_placebo_tau_hist.png", dpi=200)
    plt.close()

    return pd.DataFrame({"tau_perm": tau_perm}), {
        "sigma_u": sigma_u,
        "tau_hat_obs": tau_hat_obs,
        "empirical_two_sided_p_value": p_emp,
        "B": B,
    }


# -----------------------------------------------------------------------------
# Main script
# -----------------------------------------------------------------------------
if __name__ == "__main__":
    base = simulate_base_data(n=5000, tau=1.0, beta=1.0, seed=123)

    x_true = base["x_true"]
    d = base["d"]
    y = base["y"]
    y_placebo = base["y_placebo"]
    tau_true = base["tau_true"]
    beta_true = base["beta_true"]

    # Part 3: measurement error simulation
    results = run_measurement_error_simulation(
        x_true=x_true,
        d=d,
        y=y,
        y_placebo=y_placebo,
        tau_true=tau_true,
        beta_true=beta_true,
        sigma_u_grid=(0.0, 0.2, 0.5, 1.0, 2.0),
        R=30,
        validation_share=0.20,
        seed=999,
    )
    results.to_csv("outputs/measurement_error_results.csv", index=False)

    clean_table = results[
        [
            "sigma_u",
            "tau_oracle_mean",
            "tau_naive_mean",
            "tau_cal_mean",
            "beta_oracle_mean",
            "beta_naive_mean",
            "beta_cal_mean",
        ]
    ].copy()
    clean_table.to_csv("outputs/measurement_error_clean_table.csv", index=False)

    plot_measurement_error_results(results, tau_true=tau_true, beta_true=beta_true)

    # Part 4: validation share analysis
    validation_results = run_validation_share_analysis(
        x_true=x_true,
        d=d,
        y=y,
        sigma_u=1.0,
        validation_shares=(0.05, 0.20, 0.50),
        R=30,
        seed=2024,
    )
    validation_results.to_csv("outputs/validation_share_results.csv", index=False)

    # Part 5a: outcome placebo
    placebo_out = run_outcome_placebo(
        x_true=x_true,
        d=d,
        y_placebo=y_placebo,
        sigma_u=1.0,
        seed=555,
    )
    pd.DataFrame([placebo_out]).to_csv("outputs/outcome_placebo_result.csv", index=False)

    # Part 5b: treatment permutation placebo
    perm_df, perm_summary = run_treatment_permutation_placebo(
        x_true=x_true,
        d=d,
        y=y,
        sigma_u=1.0,
        B=500,
        seed=777,
    )
    perm_df.to_csv("outputs/permutation_tau_distribution.csv", index=False)
    pd.DataFrame([perm_summary]).to_csv("outputs/permutation_summary.csv", index=False)

    print("Done.")
    print("Wrote:")
    print("  outputs/measurement_error_results.csv")
    print("  outputs/measurement_error_clean_table.csv")
    print("  outputs/validation_share_results.csv")
    print("  outputs/outcome_placebo_result.csv")
    print("  outputs/permutation_tau_distribution.csv")
    print("  outputs/permutation_summary.csv")
    print("  figures/measurement_error_tau_vs_sigma.png")
    print("  figures/measurement_error_beta_vs_sigma.png")
    print("  figures/permutation_placebo_tau_hist.png")
