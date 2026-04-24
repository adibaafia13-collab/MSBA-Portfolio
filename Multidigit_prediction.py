# =============================================================================
# Assignment 3: Multi-Digit Number Recognition (SVHN Dataset)
# =============================================================================
# Architecture based on: Goodfellow et al. (2013) https://arxiv.org/abs/1312.6082
#
# Key idea: predict both the NUMBER OF DIGITS and each DIGIT POSITION separately.
# If the longest number in the dataset is k digits, we need k+1 softmax outputs:
#   - Output 0: length of the number (how many digits, up to k)
#   - Outputs 1..k: digit at each position (0-9, plus a "blank/none" class)
#
# Labels in digitStruct.json: digit '1'-'9' => label 1-9, '0' => label 10
# =============================================================================

import os
import json
import numpy as np
import tensorflow as tf
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from PIL import Image

# =============================================================================
# STEP 1: Configuration
# =============================================================================

TRAIN_DIR = "SVHN/train"          # path to training images + digitStruct.json
TEST_DIR  = "SVHN/test"           # path to test images + digitStruct.json

IMG_HEIGHT = 64                   # resize all images to this height
IMG_WIDTH  = 64                   # resize all images to this width
MAX_DIGITS = 5                    # longest number in SVHN is 5 digits
NUM_CLASSES = 11                  # digits 1-9 (labels 1-9), 0 (label 10), + blank (0)
BATCH_SIZE  = 32
EPOCHS      = 15
LEARNING_RATE = 0.001


# =============================================================================
# STEP 2: Parse digitStruct.json
# =============================================================================

def load_digit_struct(folder):
    """
    Load digitStruct.json from a folder.
    Returns a dict: { filename -> list of {'label': int, 'top': float, 'left': float,
                                            'height': float, 'width': float} }
    """
    json_path = os.path.join(folder, "digitStruct.json")
    with open(json_path, "r") as f:
        data = json.load(f)

    # data is a list of {"filename": "1.png", "boxes": [...]}
    result = {}
    for entry in data:
        filename = entry["filename"]
        boxes    = entry["boxes"]  # list of box dicts
        # Normalise: always a list even if single digit
        if isinstance(boxes, dict):
            boxes = [boxes]
        result[filename] = boxes
    return result


# =============================================================================
# STEP 3: Build image + label arrays
# =============================================================================

def build_dataset(folder, digit_struct, max_digits=MAX_DIGITS):
    """
    Load every image in digit_struct, resize to (IMG_HEIGHT, IMG_WIDTH),
    and build multi-output labels:
        y_length : int  - number of digits (1 to max_digits)
        y_digit1 : int  - label of digit 1  (0 = blank/none)
        y_digit2 : int  - label of digit 2
        ...
        y_digitN : int  - label of digit N
    Labels follow the original convention: '1'-'9' -> 1-9, '0' -> 10.
    A blank position (no digit) is encoded as 0.
    """
    images   = []
    lengths  = []
    digits   = [[] for _ in range(max_digits)]  # one list per position

    filenames = sorted(digit_struct.keys(),
                       key=lambda x: int(os.path.splitext(x)[0]))

    skipped = 0
    for fname in filenames:
        boxes = digit_struct[fname]
        n = len(boxes)

        # Skip images with more digits than max_digits
        if n > max_digits:
            skipped += 1
            continue

        img_path = os.path.join(folder, fname)
        try:
            img = Image.open(img_path).convert("RGB")
        except Exception:
            skipped += 1
            continue

        img = img.resize((IMG_WIDTH, IMG_HEIGHT))
        images.append(np.array(img, dtype=np.float32) / 255.0)

        lengths.append(n)

        # Fill digit labels; pad with 0 (blank) for unused positions
        for pos in range(max_digits):
            if pos < n:
                digits[pos].append(int(boxes[pos]["label"]))
            else:
                digits[pos].append(0)   # blank

    print(f"  Loaded {len(images)} images, skipped {skipped}")

    X = np.array(images, dtype=np.float32)          # (N, H, W, 3)
    y_len = np.array(lengths, dtype=np.int32)        # (N,)
    y_dig = [np.array(d, dtype=np.int32) for d in digits]  # list of (N,) arrays

    return X, y_len, y_dig


# =============================================================================
# STEP 4: Model Architecture  (multi-output CNN)
# =============================================================================
#
# Shared convolutional backbone -> GlobalAveragePooling -> Dense trunk
# Then branch into (max_digits + 1) separate softmax heads:
#   head 0: length prediction  (classes: 1 .. max_digits)
#   head i: digit at position i (classes: 0=blank, 1-9, 10=zero)

def build_model(max_digits=MAX_DIGITS, num_classes=NUM_CLASSES):
    inputs = tf.keras.layers.Input(shape=(IMG_HEIGHT, IMG_WIDTH, 3), name="image")

    # ---- Shared backbone ----
    # Block 1
    x = tf.keras.layers.Conv2D(32, 3, padding="same", activation="relu")(inputs)
    x = tf.keras.layers.Conv2D(32, 3, padding="same", activation="relu")(x)
    x = tf.keras.layers.MaxPooling2D(2, strides=2)(x)
    x = tf.keras.layers.BatchNormalization()(x)
    x = tf.keras.layers.Dropout(0.25)(x)

    # Block 2
    x = tf.keras.layers.Conv2D(64, 3, padding="same", activation="relu")(x)
    x = tf.keras.layers.Conv2D(64, 3, padding="same", activation="relu")(x)
    x = tf.keras.layers.MaxPooling2D(2, strides=2)(x)
    x = tf.keras.layers.BatchNormalization()(x)
    x = tf.keras.layers.Dropout(0.25)(x)

    # Block 3
    x = tf.keras.layers.Conv2D(128, 3, padding="same", activation="relu")(x)
    x = tf.keras.layers.Conv2D(128, 3, padding="same", activation="relu")(x)
    x = tf.keras.layers.MaxPooling2D(2, strides=2)(x)
    x = tf.keras.layers.BatchNormalization()(x)
    x = tf.keras.layers.Dropout(0.25)(x)

    # Block 4
    x = tf.keras.layers.Conv2D(256, 3, padding="same", activation="relu")(x)
    x = tf.keras.layers.MaxPooling2D(2, strides=2)(x)
    x = tf.keras.layers.BatchNormalization()(x)
    x = tf.keras.layers.Dropout(0.25)(x)

    # Trunk: flatten + dense
    x = tf.keras.layers.GlobalAveragePooling2D()(x)
    x = tf.keras.layers.Dense(1024, activation="relu")(x)
    x = tf.keras.layers.Dropout(0.5)(x)
    x = tf.keras.layers.Dense(512, activation="relu")(x)
    x = tf.keras.layers.Dropout(0.5)(x)

    # ---- Output heads ----
    # Head 0: predict number of digits  (1 to max_digits)
    length_out = tf.keras.layers.Dense(max_digits + 1, activation="softmax",
                                       name="length")(x)

    # Heads 1..max_digits: predict each digit position
    digit_outs = []
    for i in range(1, max_digits + 1):
        d_out = tf.keras.layers.Dense(num_classes, activation="softmax",
                                      name=f"digit_{i}")(x)
        digit_outs.append(d_out)

    all_outputs = [length_out] + digit_outs
    model = tf.keras.Model(inputs=inputs, outputs=all_outputs)
    return model


# =============================================================================
# STEP 5: Prepare labels for model.fit()
# =============================================================================

def prepare_labels(y_len, y_dig, max_digits=MAX_DIGITS):
    """
    Returns a list of label arrays matching the model's output order:
      [length_labels, digit1_labels, digit2_labels, ..., digitN_labels]
    """
    # Length labels: shift so classes are 0-indexed for sparse CE
    # (length 1 -> class 0, length 2 -> class 1, ..., length max_digits -> class max_digits-1)
    # We'll use classes 0..max_digits where 0 = "0 digits" (unused) and 1..max_digits = actual lengths
    # Simplest: just pass raw lengths as integers; sparse_categorical_crossentropy handles it.
    return [y_len] + list(y_dig)


# =============================================================================
# STEP 6: Compile & Train
# =============================================================================

def compile_model(model, max_digits=MAX_DIGITS):
    losses = {
        "length": "sparse_categorical_crossentropy",
    }
    loss_weights = {"length": 0.5}  # slightly lower weight on length vs digits

    for i in range(1, max_digits + 1):
        losses[f"digit_{i}"] = "sparse_categorical_crossentropy"
        loss_weights[f"digit_{i}"] = 1.0

    model.compile(
        optimizer=tf.keras.optimizers.Adam(learning_rate=LEARNING_RATE),
        loss=losses,
        loss_weights=loss_weights,
        metrics={name: "accuracy" for name in losses}
    )
    return model


# =============================================================================
# STEP 7: Evaluate — full-sequence accuracy
# =============================================================================

def full_sequence_accuracy(model, X, y_len, y_dig, batch_size=BATCH_SIZE,
                            max_digits=MAX_DIGITS):
    """
    A prediction is correct ONLY if the predicted length AND all predicted
    digit positions match the ground truth (exact match on the full number).
    """
    preds = model.predict(X, batch_size=batch_size, verbose=0)
    # preds[0] = length predictions  shape (N, max_digits+1)
    # preds[1..] = digit predictions shape (N, num_classes)

    pred_len  = np.argmax(preds[0], axis=1)      # predicted length
    pred_digs = [np.argmax(preds[i+1], axis=1) for i in range(max_digits)]

    correct = 0
    N = len(X)
    for idx in range(N):
        if pred_len[idx] != y_len[idx]:
            continue
        match = all(pred_digs[pos][idx] == y_dig[pos][idx]
                    for pos in range(max_digits))
        if match:
            correct += 1

    return correct / N


# =============================================================================
# STEP 8: Visualise bounding boxes on sample images
# =============================================================================

def visualise_bounding_boxes(folder, digit_struct, n_samples=4):
    """Draw bounding boxes on a few training images to verify the JSON data."""
    filenames = list(digit_struct.keys())[:n_samples]
    fig, axes = plt.subplots(1, n_samples, figsize=(4 * n_samples, 4))
    if n_samples == 1:
        axes = [axes]

    for ax, fname in zip(axes, filenames):
        img = Image.open(os.path.join(folder, fname)).convert("RGB")
        ax.imshow(img)
        for box in digit_struct[fname]:
            rect = patches.Rectangle(
                (box["left"], box["top"]),
                box["width"], box["height"],
                linewidth=2, edgecolor="red", facecolor="none"
            )
            ax.add_patch(rect)
            label = int(box["label"])
            display = "0" if label == 10 else str(label)
            ax.text(box["left"], box["top"] - 3, display,
                    color="red", fontsize=10, fontweight="bold")
        ax.set_title(fname)
        ax.axis("off")

    plt.tight_layout()
    plt.savefig("bounding_box_samples.png", dpi=100)
    plt.show()
    print("Saved bounding_box_samples.png")


# =============================================================================
# MAIN
# =============================================================================

if __name__ == "__main__":

    # ------------------------------------------------------------------
    # 1. Load JSON metadata
    # ------------------------------------------------------------------
    print("Loading digitStruct.json ...")
    train_struct = load_digit_struct(TRAIN_DIR)
    test_struct  = load_digit_struct(TEST_DIR)
    print(f"  Train entries: {len(train_struct)}")
    print(f"  Test  entries: {len(test_struct)}")

    # Optional: visualise a few bounding boxes
    visualise_bounding_boxes(TRAIN_DIR, train_struct, n_samples=4)

    # ------------------------------------------------------------------
    # 2. Determine max number of digits in the dataset
    # ------------------------------------------------------------------
    all_lengths = [len(v) for v in train_struct.values()] + \
                  [len(v) for v in test_struct.values()]
    dataset_max_digits = max(all_lengths)
    print(f"  Longest number in dataset: {dataset_max_digits} digits")
    # Use the larger of our constant or the actual maximum
    MAX_DIGITS = max(MAX_DIGITS, dataset_max_digits)

    # ------------------------------------------------------------------
    # 3. Build image arrays
    # ------------------------------------------------------------------
    print("\nBuilding training set ...")
    X_train, y_len_train, y_dig_train = build_dataset(TRAIN_DIR, train_struct)

    print("Building test set ...")
    X_test,  y_len_test,  y_dig_test  = build_dataset(TEST_DIR,  test_struct)

    print(f"\nX_train shape: {X_train.shape}")
    print(f"X_test  shape: {X_test.shape}")

    # ------------------------------------------------------------------
    # 4. Build & compile model
    # ------------------------------------------------------------------
    print("\nBuilding model ...")
    model = build_model(max_digits=MAX_DIGITS)
    model = compile_model(model, max_digits=MAX_DIGITS)
    model.summary()

    # ------------------------------------------------------------------
    # 5. Prepare label dicts for model.fit()
    # ------------------------------------------------------------------
    train_labels = {"length": y_len_train}
    test_labels  = {"length": y_len_test}
    for i in range(MAX_DIGITS):
        train_labels[f"digit_{i+1}"] = y_dig_train[i]
        test_labels[f"digit_{i+1}"]  = y_dig_test[i]

    # ------------------------------------------------------------------
    # 6. Callbacks
    # ------------------------------------------------------------------
    callbacks = [
        tf.keras.callbacks.EarlyStopping(
            monitor="val_loss", patience=4, restore_best_weights=True,
            verbose=1
        ),
        tf.keras.callbacks.ReduceLROnPlateau(
            monitor="val_loss", factor=0.5, patience=2, verbose=1
        ),
        tf.keras.callbacks.ModelCheckpoint(
            "best_svhn_model.keras", monitor="val_loss",
            save_best_only=True, verbose=1
        ),
    ]

    # ------------------------------------------------------------------
    # 7. Train
    # ------------------------------------------------------------------
    print("\nTraining ...")
    history = model.fit(
        x=X_train,
        y=train_labels,
        validation_data=(X_test, test_labels),
        batch_size=BATCH_SIZE,
        epochs=EPOCHS,
        callbacks=callbacks,
        verbose=1
    )

    # ------------------------------------------------------------------
    # 8. Evaluate
    # ------------------------------------------------------------------
    print("\nEvaluating ...")

    # Per-head accuracies
    results = model.evaluate(X_test, test_labels, batch_size=BATCH_SIZE, verbose=0)
    metric_names = model.metrics_names
    print("\nPer-head metrics on test set:")
    for name, val in zip(metric_names, results):
        print(f"  {name}: {val:.4f}")

    # Full-sequence (exact match) accuracy
    seq_acc = full_sequence_accuracy(model, X_test, y_len_test, y_dig_test,
                                     max_digits=MAX_DIGITS)
    print(f"\nFull-sequence (exact match) accuracy: {seq_acc:.4f}")

    # ------------------------------------------------------------------
    # 9. Plot training history
    # ------------------------------------------------------------------
    fig, axes = plt.subplots(1, 2, figsize=(12, 4))

    axes[0].plot(history.history["loss"],     label="Train loss")
    axes[0].plot(history.history["val_loss"], label="Val loss")
    axes[0].set_title("Total Loss")
    axes[0].set_xlabel("Epoch")
    axes[0].legend()

    # Plot digit_1 accuracy as a representative per-position metric
    if "digit_1_accuracy" in history.history:
        axes[1].plot(history.history["digit_1_accuracy"],     label="Train digit-1 acc")
        axes[1].plot(history.history["val_digit_1_accuracy"], label="Val digit-1 acc")
        axes[1].set_title("Digit-1 Position Accuracy")
        axes[1].set_xlabel("Epoch")
        axes[1].legend()

    plt.tight_layout()
    plt.savefig("training_history.png", dpi=100)
    plt.show()
    print("Saved training_history.png")

    # ------------------------------------------------------------------
    # 10. Save final model
    # ------------------------------------------------------------------
    model.save("svhn_multidigit_final.keras")
    print("Model saved to svhn_multidigit_final.keras")