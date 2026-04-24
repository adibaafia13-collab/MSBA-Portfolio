# Group Assignment 2
# Tuning Deep Neural Networks on Real-life Business Data
#
# Uses ONLY pricing.csv, as required by the assignment.
# Builds a TensorFlow deep neural network and tunes:
# - batch size
# - number of hidden layers
# - number of hidden neurons
# - activation function
# - optimizer
# - learning rate
# - learning rate scheduling
#
# Outputs:
# - tuning_results.csv
# - best_config.json
# - final_test_metrics.csv
# - several presentation-ready plots
# - summary text file
#
# ============================================================

# ============================================================

import os
import json
import time
import random
import itertools
import warnings
from typing import Dict, Any, List, Tuple

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import psutil
import tensorflow as tf

from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import r2_score, mean_squared_error, mean_absolute_error

warnings.filterwarnings("ignore")

# ============================================================
# Reproducibility
# ============================================================
SEED = 42
np.random.seed(SEED)
random.seed(SEED)
tf.random.set_seed(SEED)
os.environ["PYTHONHASHSEED"] = str(SEED)

# ============================================================
# Paths  ← ONLY SECTION CHANGED FOR COLAB
# ============================================================
# Update this to wherever you placed pricing.csv in your Drive:
DATA_PATH = "/content/drive/MyDrive/pricing.csv"

# Outputs will be saved here (created automatically):
OUTPUT_DIR = "/content/drive/MyDrive/project2_outputs"
os.makedirs(OUTPUT_DIR, exist_ok=True)

# ============================================================
# User-controlled options
# ============================================================
# keep False only if you explicitly want sku as a predictor
DROP_SKU = True
LOG_TRANSFORM_TARGET = True
TEST_SIZE = 0.15
VAL_SIZE_WITHIN_TRAIN = 0.1765   # makes total split roughly 70/15/15
MAX_EPOCHS = 40
EARLY_STOPPING_PATIENCE = 6

# Optional speed control:
# Set to an integer like 30 if you want to test only first 30 configs
MAX_CONFIGS_TO_RUN = 40
# None

# ============================================================
# Utility functions
# ============================================================


def set_plot_style():
    plt.rcParams["figure.figsize"] = (8, 5)
    plt.rcParams["axes.grid"] = True
    plt.rcParams["grid.alpha"] = 0.25
    plt.rcParams["font.size"] = 11


def regression_metrics(y_true: np.ndarray, y_pred: np.ndarray) -> Dict[str, float]:
    mse = mean_squared_error(y_true, y_pred)
    rmse = np.sqrt(mse)
    mae = mean_absolute_error(y_true, y_pred)
    r2 = r2_score(y_true, y_pred)
    return {
        "R2": r2,
        "MSE": mse,
        "RMSE": rmse,
        "MAE": mae
    }


def inverse_target_transform(y: np.ndarray) -> np.ndarray:
    if LOG_TRANSFORM_TARGET:
        return np.expm1(y)
    return y


def safe_filename(text: str) -> str:
    return "".join(c if c.isalnum() or c in ("_", "-", ".") else "_" for c in str(text))

# ============================================================
# Data loading and preprocessing
# ============================================================


def load_data(data_path: str) -> pd.DataFrame:
    df = pd.read_csv(data_path)
    print("Loaded pricing.csv")
    print("Shape:", df.shape)
    print("Columns:", list(df.columns))
    return df


def split_data(df: pd.DataFrame) -> Tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    """
    Split into train / validation / test using only pricing.csv.
    First split off test, then split remaining into train and validation.
    """
    train_val_df, test_df = train_test_split(
        df,
        test_size=TEST_SIZE,
        random_state=SEED,
        shuffle=True
    )

    train_df, val_df = train_test_split(
        train_val_df,
        test_size=VAL_SIZE_WITHIN_TRAIN,
        random_state=SEED,
        shuffle=True
    )

    print("\nData split:")
    print("Train shape:", train_df.shape)
    print("Val shape  :", val_df.shape)
    print("Test shape :", test_df.shape)

    return train_df.copy(), val_df.copy(), test_df.copy()


def preprocess_splits(
    train_df: pd.DataFrame,
    val_df: pd.DataFrame,
    test_df: pd.DataFrame,
    drop_sku: bool = True
) -> Tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray, np.ndarray, np.ndarray, List[str]]:
    """
    Preprocess based on TRAIN only, then apply same transformations to val/test.
    """
    train_df = train_df.copy()
    val_df = val_df.copy()
    test_df = test_df.copy()

    # Drop SKU if treated as identifier only
    if drop_sku and "sku" in train_df.columns:
        train_df.drop(columns=["sku"], inplace=True)
        val_df.drop(columns=["sku"], inplace=True)
        test_df.drop(columns=["sku"], inplace=True)

    # Target
    y_train_original = train_df["quantity"].values.astype(np.float32)
    y_val_original = val_df["quantity"].values.astype(np.float32)
    y_test_original = test_df["quantity"].values.astype(np.float32)

    if LOG_TRANSFORM_TARGET:
        y_train = np.log1p(y_train_original).astype(np.float32).reshape(-1, 1)
        y_val = np.log1p(y_val_original).astype(np.float32).reshape(-1, 1)
        y_test = np.log1p(y_test_original).astype(np.float32).reshape(-1, 1)
    else:
        y_train = y_train_original.astype(np.float32).reshape(-1, 1)
        y_val = y_val_original.astype(np.float32).reshape(-1, 1)
        y_test = y_test_original.astype(np.float32).reshape(-1, 1)

    # Drop target from predictors
    X_train_df = train_df.drop(columns=["quantity"])
    X_val_df = val_df.drop(columns=["quantity"])
    X_test_df = test_df.drop(columns=["quantity"])

    # Numeric transformations
    numeric_cols = [col for col in ["price", "order",
                                    "duration"] if col in X_train_df.columns]

    for col in numeric_cols:
        X_train_df[col] = np.log1p(X_train_df[col])
        X_val_df[col] = np.log1p(X_val_df[col])
        X_test_df[col] = np.log1p(X_test_df[col])

    scaler = StandardScaler()
    X_train_df[numeric_cols] = scaler.fit_transform(X_train_df[numeric_cols])
    X_val_df[numeric_cols] = scaler.transform(X_val_df[numeric_cols])
    X_test_df[numeric_cols] = scaler.transform(X_test_df[numeric_cols])

    # One-hot encode category
    if "category" in X_train_df.columns:
        X_train_df = pd.get_dummies(
            X_train_df, columns=["category"], prefix="cat")
        X_val_df = pd.get_dummies(X_val_df, columns=["category"], prefix="cat")
        X_test_df = pd.get_dummies(
            X_test_df, columns=["category"], prefix="cat")

        # Align columns to train
        X_val_df = X_val_df.reindex(columns=X_train_df.columns, fill_value=0)
        X_test_df = X_test_df.reindex(columns=X_train_df.columns, fill_value=0)

    # Convert dummy columns to int
    dummy_cols = [c for c in X_train_df.columns if c.startswith("cat_")]
    for col in dummy_cols:
        X_train_df[col] = X_train_df[col].astype(int)
        X_val_df[col] = X_val_df[col].astype(int)
        X_test_df[col] = X_test_df[col].astype(int)

    feature_names = list(X_train_df.columns)

    X_train = X_train_df.values.astype(np.float32)
    X_val = X_val_df.values.astype(np.float32)
    X_test = X_test_df.values.astype(np.float32)

    print("\nPreprocessing complete.")
    print("X_train shape:", X_train.shape)
    print("X_val shape  :", X_val.shape)
    print("X_test shape :", X_test.shape)

    return X_train, X_val, X_test, y_train, y_val, y_test, feature_names

# ============================================================
# Model building
# ============================================================


def get_learning_rate(config: Dict[str, Any]):
    lr = config["learning_rate"]

    if config["lr_schedule"]:
        return tf.keras.optimizers.schedules.ExponentialDecay(
            initial_learning_rate=lr,
            decay_steps=1000,
            decay_rate=0.96,
            staircase=True
        )
    return lr


def get_optimizer(name: str, learning_rate):
    name = name.lower()

    if name == "sgd":
        return tf.keras.optimizers.SGD(learning_rate=learning_rate)
    elif name == "momentum":
        return tf.keras.optimizers.SGD(learning_rate=learning_rate, momentum=0.9)
    elif name == "nesterov":
        return tf.keras.optimizers.SGD(learning_rate=learning_rate, momentum=0.9, nesterov=True)
    elif name == "adagrad":
        return tf.keras.optimizers.Adagrad(learning_rate=learning_rate)
    elif name == "rmsprop":
        return tf.keras.optimizers.RMSprop(learning_rate=learning_rate)
    elif name == "adam":
        return tf.keras.optimizers.Adam(learning_rate=learning_rate)
    else:
        raise ValueError(f"Unsupported optimizer: {name}")


def add_dense_block(model: tf.keras.Sequential, units: int, activation_name: str, dropout_rate: float):
    activation_name = activation_name.lower()

    if activation_name in ["sigmoid", "tanh", "relu", "elu"]:
        model.add(tf.keras.layers.Dense(units, activation=activation_name))
    elif activation_name == "leaky_relu":
        model.add(tf.keras.layers.Dense(units))
        model.add(tf.keras.layers.LeakyReLU(negative_slope=0.1))
    elif activation_name == "prelu":
        model.add(tf.keras.layers.Dense(units))
        model.add(tf.keras.layers.PReLU())
    else:
        raise ValueError(f"Unsupported activation: {activation_name}")

    if dropout_rate > 0:
        model.add(tf.keras.layers.Dropout(dropout_rate))


def build_model(input_dim: int, config: Dict[str, Any]) -> tf.keras.Model:
    model = tf.keras.Sequential()
    model.add(tf.keras.layers.Input(shape=(input_dim,)))

    for units in config["hidden_units"]:
        add_dense_block(
            model=model,
            units=units,
            activation_name=config["activation"],
            dropout_rate=config["dropout_rate"]
        )

    model.add(tf.keras.layers.Dense(1))

    learning_rate = get_learning_rate(config)
    optimizer = get_optimizer(config["optimizer"], learning_rate)

    model.compile(
        optimizer=optimizer,
        loss="mse",
        metrics=["mse"]
    )

    return model

# ============================================================
# Hyperparameter configuration
# ============================================================


def generate_hidden_units(num_layers: int, base_units: int) -> List[int]:
    """
    Example:
    3 layers, 128 base -> [128, 64, 32]
    4 layers, 64 base  -> [64, 32, 16, 8]
    """
    units = []
    current = base_units
    for _ in range(num_layers):
        units.append(max(current, 8))
        current = max(current // 2, 8)
    return units


def create_search_space() -> List[Dict[str, Any]]:
    batch_sizes = [64, 128, 256]
    hidden_layers_list = [2, 3, 4]
    base_units_list = [64, 128]
    activations = ["sigmoid", "tanh", "relu", "leaky_relu", "prelu", "elu"]
    optimizers = ["sgd", "momentum", "nesterov", "adagrad", "rmsprop", "adam"]
    learning_rates = [0.001, 0.0005]
    lr_schedule_options = [False, True]
    dropout_rates = [0.0, 0.2]

    configs = []

    for (
        batch_size,
        hidden_layers,
        base_units,
        activation,
        optimizer,
        learning_rate,
        lr_schedule,
        dropout_rate
    ) in itertools.product(
        batch_sizes,
        hidden_layers_list,
        base_units_list,
        activations,
        optimizers,
        learning_rates,
        lr_schedule_options,
        dropout_rates
    ):
        configs.append({
            "batch_size": batch_size,
            "hidden_layers": hidden_layers,
            "hidden_units": generate_hidden_units(hidden_layers, base_units),
            "base_units": base_units,
            "activation": activation,
            "optimizer": optimizer,
            "learning_rate": learning_rate,
            "lr_schedule": lr_schedule,
            "dropout_rate": dropout_rate,
            "epochs": MAX_EPOCHS
        })

    if MAX_CONFIGS_TO_RUN is not None:
        configs = configs[:MAX_CONFIGS_TO_RUN]

    return configs

# ============================================================
# Training one configuration
# ============================================================


def train_one_config(
    X_train: np.ndarray,
    y_train: np.ndarray,
    X_val: np.ndarray,
    y_val: np.ndarray,
    config: Dict[str, Any]
):
    tf.keras.backend.clear_session()

    model = build_model(X_train.shape[1], config)

    callbacks = [
        tf.keras.callbacks.EarlyStopping(
            monitor="val_loss",
            patience=EARLY_STOPPING_PATIENCE,
            restore_best_weights=True
        )
    ]

    start_time = time.time()

    history = model.fit(
        X_train,
        y_train,
        validation_data=(X_val, y_val),
        epochs=config["epochs"],
        batch_size=config["batch_size"],
        verbose=0,
        callbacks=callbacks
    )

    training_time = time.time() - start_time

    # Validation prediction in original target scale
    y_val_pred_transformed = model.predict(X_val, verbose=0).flatten()
    y_val_pred = inverse_target_transform(y_val_pred_transformed)
    y_val_true = inverse_target_transform(y_val.flatten())

    metrics = regression_metrics(y_val_true, y_val_pred)

    process = psutil.Process()
    ram_usage_mb = process.memory_info().rss / 1024**2

    result = {
        "batch_size": config["batch_size"],
        "hidden_layers": config["hidden_layers"],
        "hidden_units": str(config["hidden_units"]),
        "base_units": config["base_units"],
        "activation": config["activation"],
        "optimizer": config["optimizer"],
        "learning_rate": config["learning_rate"],
        "lr_schedule": config["lr_schedule"],
        "dropout_rate": config["dropout_rate"],
        "epochs_requested": config["epochs"],
        "epochs_trained": len(history.history["loss"]),
        "training_time_sec": training_time,
        "ram_usage_mb": ram_usage_mb,
        "best_val_loss": float(np.min(history.history["val_loss"])),
        "val_R2": metrics["R2"],
        "val_MSE": metrics["MSE"],
        "val_RMSE": metrics["RMSE"],
        "val_MAE": metrics["MAE"]
    }

    return model, history, result

# ============================================================
# Tuning function required by assignment
# ============================================================


def tune_parameters(
    X_train: np.ndarray,
    y_train: np.ndarray,
    X_val: np.ndarray,
    y_val: np.ndarray,
    configs: List[Dict[str, Any]]
):
    results = []
    best_model = None
    best_history = None
    best_config = None
    best_val_r2 = -np.inf

    print(f"\nStarting tuning over {len(configs)} configurations...\n")

    for i, config in enumerate(configs, start=1):
        print(f"Trial {i}/{len(configs)}")
        print(config)

        try:
            model, history, result = train_one_config(
                X_train, y_train, X_val, y_val, config
            )
            results.append(result)

            print(f"Validation R2: {result['val_R2']:.4f}")
            print(f"Validation RMSE: {result['val_RMSE']:.4f}")
            print("-" * 70)

            if result["val_R2"] > best_val_r2:
                best_val_r2 = result["val_R2"]
                best_model = model
                best_history = history
                best_config = config

        except Exception as e:
            print(f"Configuration failed: {e}")
            print("-" * 70)

            fail_result = {
                "batch_size": config["batch_size"],
                "hidden_layers": config["hidden_layers"],
                "hidden_units": str(config["hidden_units"]),
                "base_units": config["base_units"],
                "activation": config["activation"],
                "optimizer": config["optimizer"],
                "learning_rate": config["learning_rate"],
                "lr_schedule": config["lr_schedule"],
                "dropout_rate": config["dropout_rate"],
                "epochs_requested": config["epochs"],
                "epochs_trained": np.nan,
                "training_time_sec": np.nan,
                "ram_usage_mb": np.nan,
                "best_val_loss": np.nan,
                "val_R2": np.nan,
                "val_MSE": np.nan,
                "val_RMSE": np.nan,
                "val_MAE": np.nan
            }
            results.append(fail_result)

    results_df = pd.DataFrame(results).sort_values(
        by="val_R2", ascending=False).reset_index(drop=True)
    return results_df, best_model, best_history, best_config

# ============================================================
# Final retraining on train+val and test evaluation
# ============================================================


def retrain_best_model_on_trainval(
    X_train: np.ndarray,
    y_train: np.ndarray,
    X_val: np.ndarray,
    y_val: np.ndarray,
    best_config: Dict[str, Any]
):
    tf.keras.backend.clear_session()

    X_trainval = np.vstack([X_train, X_val])
    y_trainval = np.vstack([y_train, y_val])

    model = build_model(X_trainval.shape[1], best_config)

    callbacks = [
        tf.keras.callbacks.EarlyStopping(
            monitor="loss",
            patience=EARLY_STOPPING_PATIENCE,
            restore_best_weights=True
        )
    ]

    start_time = time.time()

    history = model.fit(
        X_trainval,
        y_trainval,
        epochs=best_config["epochs"],
        batch_size=best_config["batch_size"],
        verbose=0,
        callbacks=callbacks
    )

    training_time = time.time() - start_time
    return model, history, training_time


def evaluate_on_test(model: tf.keras.Model, X_test: np.ndarray, y_test: np.ndarray):
    y_test_pred_transformed = model.predict(X_test, verbose=0).flatten()

    y_test_pred = inverse_target_transform(y_test_pred_transformed)
    y_test_true = inverse_target_transform(y_test.flatten())

    metrics = regression_metrics(y_test_true, y_test_pred)
    return metrics, y_test_true, y_test_pred

# ============================================================
# Plotting
# ============================================================


def save_learning_curve(history, output_path: str, title: str):
    set_plot_style()
    plt.figure()

    if "loss" in history.history:
        plt.plot(history.history["loss"], label="Train Loss")
    if "val_loss" in history.history:
        plt.plot(history.history["val_loss"], label="Validation Loss")

    plt.xlabel("Epoch")
    plt.ylabel("MSE Loss")
    plt.title(title)
    plt.legend()
    plt.tight_layout()
    plt.savefig(output_path, dpi=300)
    plt.show()
    plt.close()


def save_grouped_bar(df: pd.DataFrame, x_col: str, y_col: str, title: str, output_path: str):
    set_plot_style()
    summary = df.groupby(x_col)[y_col].mean().sort_values(ascending=False)

    plt.figure()
    summary.plot(kind="bar")
    plt.title(title)
    plt.xlabel(x_col)
    plt.ylabel(f"Average {y_col}")
    plt.xticks(rotation=45)
    plt.tight_layout()
    plt.savefig(output_path, dpi=300)
    plt.show()
    plt.close()


def save_line_sensitivity(df: pd.DataFrame, x_col: str, y_col: str, title: str, output_path: str):
    set_plot_style()
    summary = df.groupby(x_col)[y_col].mean().reset_index()

    plt.figure()
    plt.plot(summary[x_col], summary[y_col], marker="o")
    plt.title(title)
    plt.xlabel(x_col)
    plt.ylabel(f"Average {y_col}")
    plt.tight_layout()
    plt.savefig(output_path, dpi=300)
    plt.show()
    plt.close()


def save_top_models_plot(df: pd.DataFrame, output_path: str):
    set_plot_style()
    top = df.head(10).copy()
    top["model_id"] = [f"M{i+1}" for i in range(len(top))]

    plt.figure(figsize=(10, 5))
    plt.bar(top["model_id"], top["val_R2"])
    plt.xlabel("Top Configurations")
    plt.ylabel("Validation R²")
    plt.title("Top 10 Configurations by Validation R²")
    plt.tight_layout()
    plt.savefig(output_path, dpi=300)
    plt.show()
    plt.close()


def save_actual_vs_predicted_plot(y_true: np.ndarray, y_pred: np.ndarray, output_path: str):
    set_plot_style()
    plt.figure()
    plt.scatter(y_true, y_pred, alpha=0.4)
    min_val = min(float(np.min(y_true)), float(np.min(y_pred)))
    max_val = max(float(np.max(y_true)), float(np.max(y_pred)))
    plt.plot([min_val, max_val], [min_val, max_val], linestyle="--")
    plt.xlabel("Actual Quantity")
    plt.ylabel("Predicted Quantity")
    plt.title("Actual vs Predicted Quantity on Test Set")
    plt.tight_layout()
    plt.savefig(output_path, dpi=300)
    plt.show()
    plt.close()

# ============================================================
# Main
# ============================================================


def main():
    print("=" * 70)
    print("PROJECT 2: TUNING DEEP NEURAL NETWORKS")
    print("=" * 70)

    # 1. Load
    df = load_data(DATA_PATH)

    # 2. Split
    train_df, val_df, test_df = split_data(df)

    # 3. Preprocess
    X_train, X_val, X_test, y_train, y_val, y_test, feature_names = preprocess_splits(
        train_df, val_df, test_df, drop_sku=DROP_SKU
    )

    # 4. Search space
    configs = create_search_space()
    print(f"\nNumber of configurations to run: {len(configs)}")

    # 5. Tune
    results_df, best_model, best_history, best_config = tune_parameters(
        X_train, y_train, X_val, y_val, configs
    )

    # 6. Save tuning results
    tuning_results_path = os.path.join(OUTPUT_DIR, "tuning_results.csv")
    results_df.to_csv(tuning_results_path, index=False)

    best_config_path = os.path.join(OUTPUT_DIR, "best_config.json")
    with open(best_config_path, "w") as f:
        json.dump(best_config, f, indent=4)

    print("\nBest configuration found:")
    print(json.dumps(best_config, indent=4))

    # 7. Save learning curve for best validation model
    save_learning_curve(
        history=best_history,
        output_path=os.path.join(
            OUTPUT_DIR, "best_validation_model_learning_curve.png"),
        title="Learning Curve of Best Validation Model"
    )

    # 8. Presentation-ready sensitivity plots
    valid_results = results_df.dropna(subset=["val_R2"]).copy()

    save_grouped_bar(
        valid_results, "activation", "val_R2",
        "Average Validation R² by Activation Function",
        os.path.join(OUTPUT_DIR, "activation_sensitivity.png")
    )

    save_grouped_bar(
        valid_results, "optimizer", "val_R2",
        "Average Validation R² by Optimizer",
        os.path.join(OUTPUT_DIR, "optimizer_sensitivity.png")
    )

    save_grouped_bar(
        valid_results, "batch_size", "val_R2",
        "Average Validation R² by Batch Size",
        os.path.join(OUTPUT_DIR, "batch_size_sensitivity.png")
    )

    save_grouped_bar(
        valid_results, "lr_schedule", "val_R2",
        "Average Validation R² by Learning Rate Scheduling",
        os.path.join(OUTPUT_DIR, "lr_schedule_sensitivity.png")
    )

    save_grouped_bar(
        valid_results, "dropout_rate", "val_R2",
        "Average Validation R² by Dropout Rate",
        os.path.join(OUTPUT_DIR, "dropout_sensitivity.png")
    )

    save_line_sensitivity(
        valid_results, "hidden_layers", "val_R2",
        "Average Validation R² by Number of Hidden Layers",
        os.path.join(OUTPUT_DIR, "hidden_layers_sensitivity.png")
    )

    save_line_sensitivity(
        valid_results, "learning_rate", "val_R2",
        "Average Validation R² by Learning Rate",
        os.path.join(OUTPUT_DIR, "learning_rate_sensitivity.png")
    )

    save_line_sensitivity(
        valid_results, "base_units", "val_R2",
        "Average Validation R² by Base Hidden Units",
        os.path.join(OUTPUT_DIR, "neurons_sensitivity.png")
    )

    save_top_models_plot(
        valid_results,
        os.path.join(OUTPUT_DIR, "top10_configurations.png")
    )

    # 9. Retrain best config on train+val
    final_model, final_history, final_trainval_time = retrain_best_model_on_trainval(
        X_train, y_train, X_val, y_val, best_config
    )

    save_learning_curve(
        history=final_history,
        output_path=os.path.join(
            OUTPUT_DIR, "final_trainval_learning_curve.png"),
        title="Learning Curve After Retraining on Train + Validation"
    )

    # 10. Final test evaluation
    test_metrics, y_test_true, y_test_pred = evaluate_on_test(
        final_model, X_test, y_test)

    final_test_metrics_path = os.path.join(
        OUTPUT_DIR, "final_test_metrics.csv")
    pd.DataFrame([test_metrics]).to_csv(final_test_metrics_path, index=False)

    predictions_path = os.path.join(OUTPUT_DIR, "test_predictions.csv")
    pd.DataFrame({
        "actual_quantity": y_test_true,
        "predicted_quantity": y_test_pred
    }).to_csv(predictions_path, index=False)

    save_actual_vs_predicted_plot(
        y_test_true, y_test_pred,
        os.path.join(OUTPUT_DIR, "actual_vs_predicted_test.png")
    )

    # 11. Save summary text
    process = psutil.Process()
    final_ram_usage_mb = process.memory_info().rss / 1024**2

    summary_path = os.path.join(OUTPUT_DIR, "project2_summary.txt")
    with open(summary_path, "w", encoding="utf-8") as f:
        f.write("PROJECT 2 SUMMARY\n")
        f.write("=" * 60 + "\n\n")
        f.write("Assignment focus:\n")
        f.write(
            "Sensitivity of deep neural network performance to tuning parameters.\n\n")

        f.write("Data used:\n")
        f.write("- pricing.csv only\n")
        f.write("- Split into train / validation / test\n\n")

        f.write("Feature handling:\n")
        f.write(f"- sku dropped: {DROP_SKU}\n")
        f.write("- log1p applied to price, order, duration\n")
        f.write("- standard scaling applied to numeric predictors\n")
        f.write("- category one-hot encoded\n")
        f.write(f"- target log-transformed: {LOG_TRANSFORM_TARGET}\n\n")

        f.write("Tuned parameters:\n")
        f.write("- batch size\n")
        f.write("- hidden layers\n")
        f.write("- hidden neurons\n")
        f.write("- activation function\n")
        f.write("- optimizer\n")
        f.write("- learning rate\n")
        f.write("- learning rate scheduling\n")
        f.write("- dropout\n\n")

        f.write("Best configuration:\n")
        f.write(json.dumps(best_config, indent=4))
        f.write("\n\n")

        f.write("Final test metrics:\n")
        for k, v in test_metrics.items():
            f.write(f"{k}: {v:.6f}\n")

        f.write("\nAdditional details:\n")
        f.write(f"- Number of tuning configurations run: {len(configs)}\n")
        f.write(
            f"- Final retraining time on train+val (sec): {final_trainval_time:.2f}\n")
        f.write(f"- Final RAM usage (MB): {final_ram_usage_mb:.2f}\n")

    # 12. Console summary
    print("\n" + "=" * 70)
    print("FINAL TEST RESULTS")
    print("=" * 70)
    for k, v in test_metrics.items():
        print(f"{k}: {v:.4f}")

    print("\nAll outputs saved to:")
    print(OUTPUT_DIR)


# ============================================================
# Run — compatible with both Colab notebooks and .py scripts
# ============================================================
main()