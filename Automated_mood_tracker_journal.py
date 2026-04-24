####################################################
## mood_tracker.py (still ongoing project)
## Mood Journal & Emotion Tracker
## Generative AI System — 7-box architecture
####################################################

import csv
import json
import os
import time
from collections import Counter
from datetime import datetime, timedelta

from storage import load_journal, save_entry, load_profile, setup_profile
from tips import get_tips


####################################################
## CONFIG
####################################################

config = {
    "model": "qwen2.5:7b",
    "max_steps": 6,
    "max_retries": 2,
    "timeout_s": 120.0,
    "temperature_first": 0.3,
    "temperature_retry": 0.0,
    "num_predict": 180,
    "memory_window": 3,
    "tips_per_entry": 3,
    "use_rag": True,
    "csv_dataset_path": "Combined Data.csv",
    "embedding_model": "BAAI/bge-base-en-v1.5",
    "rag_top_k": 3,
    "rag_max_chars": 1200,
    "rag_max_rows": 15000,
    "follow_up_prompt": "Is there anything else you want to share? (yes/no): ",
}

VALID_EMOTIONS = {"happy", "sad", "stressed", "neutral"}

EMOTION_COLORS = {
    "happy": "#4ade80",
    "neutral": "#94a3b8",
    "sad": "#60a5fa",
    "stressed": "#f87171",
    None: "#1e293b",
}

EMOTION_EMOJI = {
    "happy": "😊",
    "neutral": "😐",
    "sad": "😔",
    "stressed": "😟",
    None: "—",
}


####################################################
## GLOBAL RAG STATE
####################################################

RAG_EMBEDDER = None
RAG_INDEX = None
RAG_ROWS = []
RAG_READY = False
RAG_STATUS = "RAG not initialized."


####################################################
## DASHBOARD HELPERS
####################################################

def get_week_buckets(journal):
    """
    Returns last 7 days as ordered list of (date_str, emotion_list).
    Days with no entries still appear (empty list).
    """
    today = datetime.now().date()
    buckets = {}
    for i in range(6, -1, -1):
        d = today - timedelta(days=i)
        buckets[d.isoformat()] = []

    for entry in journal:
        try:
            ts = datetime.fromisoformat(entry["timestamp"]).date()
            key = ts.isoformat()
            if key in buckets:
                buckets[key].append(entry["emotion"])
        except Exception:
            continue

    return list(buckets.items())


def dominant_emotion(emotions):
    if not emotions:
        return None
    return Counter(emotions).most_common(1)[0][0]


def build_html(journal, profile):
    name = profile["name"] if profile else "Friend"
    total = len(journal)
    counts = Counter(e["emotion"] for e in journal)
    week = get_week_buckets(journal)

    week_labels = [d[5:] for d, _ in week]
    week_emotions = [dominant_emotion(em) for _, em in week]
    week_colors = [EMOTION_COLORS[e] for e in week_emotions]
    week_emojis = [EMOTION_EMOJI[e] for e in week_emotions]
    week_counts = [len(em) for _, em in week]

    recent = journal[-10:][::-1]

    def entry_row(entry):
        color = EMOTION_COLORS.get(entry["emotion"], "#94a3b8")
        emoji = EMOTION_EMOJI.get(entry["emotion"], "")
        ts = entry["timestamp"][:10]
        tips_html = "".join(
            f'<span class="tip-tag">{tip}</span>' for tip in entry.get("tips", [])
        )
        return f"""
        <div class="entry-card" style="border-left: 4px solid {color}">
          <div class="entry-header">
            <span class="entry-emotion" style="color:{color}">{emoji} {entry['emotion']}</span>
            <span class="entry-date">{ts}</span>
          </div>
          <p class="entry-text">"{entry['entry']}"</p>
          <p class="entry-rec">💬 {entry['recommendation']}</p>
          <div class="tips-row">{tips_html}</div>
        </div>"""

    entries_html = "\n".join(entry_row(entry) for entry in recent)

    def pill(emotion, count):
        color = EMOTION_COLORS[emotion]
        emoji = EMOTION_EMOJI[emotion]
        pct = round(count / total * 100) if total else 0
        return f"""
        <div class="pill" style="background:{color}22; border:1px solid {color}">
          <span style="color:{color}; font-size:1.4rem">{emoji}</span>
          <div>
            <div style="font-weight:700; color:{color}">{emotion}</div>
            <div style="font-size:0.8rem; color:#94a3b8">{count} entries · {pct}%</div>
          </div>
        </div>"""

    pills_html = "\n".join(pill(em, cnt) for em, cnt in counts.most_common())
    max_count = max(week_counts) if any(week_counts) else 1

    def week_bar(label, color, emoji, count):
        height_pct = int((count / max_count) * 100) if count > 0 else 0
        height_px = max(height_pct * 1.2, 4)
        tooltip = f"{count} entr{'y' if count == 1 else 'ies'}"
        return f"""
        <div class="week-col">
          <div class="bar-wrap">
            <div class="bar" style="height:{height_px}px; background:{color}"
                 title="{tooltip}"></div>
          </div>
          <div class="week-emoji">{emoji}</div>
          <div class="week-label">{label}</div>
        </div>"""

    bars_html = "\n".join(
        week_bar(week_labels[i], week_colors[i], week_emojis[i], week_counts[i])
        for i in range(7)
    )

    goals = ", ".join(profile.get("goals", [])) if profile else "—"
    triggers = ", ".join(profile.get("stress_triggers", [])) if profile else "—"

    return f"""<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Mood Journal — {name}</title>
  <style>
    * {{ box-sizing: border-box; margin: 0; padding: 0; }}
    body {{
      background: #0f172a; color: #e2e8f0;
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
      padding: 2rem;
    }}
    h1 {{ font-size: 1.8rem; font-weight: 700; margin-bottom: 0.3rem; }}
    .subtitle {{ color: #64748b; font-size: 0.95rem; margin-bottom: 2rem; }}
    .grid {{ display: grid; grid-template-columns: 1fr 1fr; gap: 1.5rem; margin-bottom: 1.5rem; }}
    .card {{
      background: #1e293b; border-radius: 12px; padding: 1.5rem;
      border: 1px solid #334155;
    }}
    .card h2 {{ font-size: 1rem; color: #94a3b8; margin-bottom: 1rem; text-transform: uppercase; letter-spacing: 0.05em; }}
    .stat-big {{ font-size: 3rem; font-weight: 800; color: #e2e8f0; }}
    .pills {{ display: flex; flex-wrap: wrap; gap: 0.75rem; }}
    .pill {{
      display: flex; align-items: center; gap: 0.6rem;
      padding: 0.6rem 1rem; border-radius: 999px;
    }}
    .week-chart {{ display: flex; align-items: flex-end; gap: 0.5rem; height: 140px; }}
    .week-col {{ display: flex; flex-direction: column; align-items: center; flex: 1; }}
    .bar-wrap {{ flex: 1; display: flex; align-items: flex-end; width: 100%; justify-content: center; }}
    .bar {{ width: 80%; border-radius: 4px 4px 0 0; min-height: 4px; transition: height 0.3s; }}
    .week-emoji {{ font-size: 1.1rem; margin-top: 4px; }}
    .week-label {{ font-size: 0.7rem; color: #64748b; margin-top: 2px; }}
    .entry-card {{
      background: #1e293b; border-radius: 10px; padding: 1rem 1.2rem;
      margin-bottom: 0.9rem; border: 1px solid #334155;
    }}
    .entry-header {{ display: flex; justify-content: space-between; margin-bottom: 0.4rem; }}
    .entry-emotion {{ font-weight: 700; text-transform: capitalize; }}
    .entry-date {{ color: #64748b; font-size: 0.85rem; }}
    .entry-text {{ color: #cbd5e1; font-style: italic; margin-bottom: 0.4rem; }}
    .entry-rec {{ color: #94a3b8; font-size: 0.88rem; margin-bottom: 0.5rem; }}
    .tips-row {{ display: flex; flex-wrap: wrap; gap: 0.4rem; }}
    .tip-tag {{
      background: #0f172a; color: #64748b; font-size: 0.75rem;
      padding: 2px 8px; border-radius: 999px; border: 1px solid #334155;
    }}
    .profile-row {{ display: flex; gap: 2rem; font-size: 0.9rem; }}
    .profile-item {{ color: #94a3b8; }}
    .profile-item span {{ color: #e2e8f0; font-weight: 600; }}
    @media (max-width: 700px) {{ .grid {{ grid-template-columns: 1fr; }} }}
  </style>
</head>
<body>
  <h1>📓 {name}'s Mood Journal</h1>
  <p class="subtitle">Generated {datetime.now().strftime('%B %d, %Y at %H:%M')}</p>

  <div class="grid">

    <div class="card">
      <h2>Total Entries</h2>
      <div class="stat-big">{total}</div>
    </div>

    <div class="card">
      <h2>Profile</h2>
      <div class="profile-row">
        <div class="profile-item">Goals<br><span>{goals}</span></div>
        <div class="profile-item">Stress triggers<br><span>{triggers}</span></div>
      </div>
    </div>

  </div>

  <div class="grid">

    <div class="card">
      <h2>Last 7 Days</h2>
      <div class="week-chart">
        {bars_html}
      </div>
    </div>

    <div class="card">
      <h2>Emotion Breakdown</h2>
      <div class="pills">
        {pills_html}
      </div>
    </div>

  </div>

  <div class="card">
    <h2>Recent Entries</h2>
    {entries_html if entries_html else '<p style="color:#64748b">No entries yet.</p>'}
  </div>

</body>
</html>"""


####################################################
## BOX 3 — LLM CALL (Ollama, no external dependency)
####################################################

def ollama_chat(model, messages, options):
    import urllib.request

    payload = json.dumps({
        "model": model,
        "messages": messages,
        "stream": False,
        "options": options,
    }).encode()

    req = urllib.request.Request(
        "http://localhost:11434/api/chat",
        data=payload,
        headers={"Content-Type": "application/json"},
        method="POST",
    )
    with urllib.request.urlopen(req, timeout=config["timeout_s"]) as resp:
        body = json.loads(resp.read().decode())

    return {"text": body["message"]["content"].strip()}


####################################################
## RAG HELPERS
####################################################

def load_csv_documents(csv_path, max_rows=None):
    """
    Load statement/label rows from a CSV file.
    Expected columns:
      - statement
      - status
    """
    rows = []

    if not os.path.exists(csv_path):
        return rows

    with open(csv_path, "r", encoding="utf-8-sig", newline="") as f:
        reader = csv.DictReader(f)

        for i, row in enumerate(reader):
            statement = str(row.get("statement", "")).strip()
            status = str(row.get("status", "")).strip()

            if not statement:
                continue

            rows.append({
                "row_id": i,
                "source": os.path.basename(csv_path),
                "text": statement,
                "label": status or "Unknown",
            })

            if max_rows is not None and len(rows) >= max_rows:
                break

    return rows


def build_faiss_index(texts, embedder):
    """
    Build a cosine-similarity FAISS index on normalized embeddings.
    """
    import faiss
    import numpy as np

    X = embedder.encode(
        texts,
        normalize_embeddings=True,
        show_progress_bar=False,
    )
    X = np.asarray(X, dtype=np.float32)

    index = faiss.IndexFlatIP(X.shape[1])
    index.add(X)
    return index


def init_rag():
    """
    Build the embedder and vector index once.
    If dependencies or data are missing, the app falls back gracefully.
    """
    global RAG_EMBEDDER, RAG_INDEX, RAG_ROWS, RAG_READY, RAG_STATUS

    if not config["use_rag"]:
        RAG_READY = False
        RAG_STATUS = "RAG disabled in config."
        return

    if RAG_READY:
        return

    try:
        from sentence_transformers import SentenceTransformer
    except Exception:
        RAG_READY = False
        RAG_STATUS = (
            "RAG disabled: install sentence-transformers, faiss-cpu, and numpy "
            "to enable retrieval."
        )
        return

    rows = load_csv_documents(
        config["csv_dataset_path"],
        max_rows=config["rag_max_rows"],
    )
    if not rows:
        RAG_READY = False
        RAG_STATUS = f"RAG disabled: no usable rows found in '{config['csv_dataset_path']}'."
        return

    texts = [row["text"] for row in rows]

    try:
        embedder = SentenceTransformer(config["embedding_model"])
        index = build_faiss_index(texts, embedder)
    except Exception as e:
        RAG_READY = False
        RAG_STATUS = f"RAG initialization failed: {e}"
        return

    RAG_EMBEDDER = embedder
    RAG_INDEX = index
    RAG_ROWS = rows
    RAG_READY = True
    RAG_STATUS = (
        f"RAG ready: indexed {len(rows)} rows from '{config['csv_dataset_path']}'."
    )


def retrieve_rag_context(query, k=None):
    """
    Retrieve the most semantically similar statements from the CSV dataset.
    """
    if not RAG_READY:
        return "Retrieved support examples:\n  (RAG unavailable; using profile, history, and tips only)\n"

    import numpy as np

    if k is None:
        k = config["rag_top_k"]

    qv = RAG_EMBEDDER.encode([query], normalize_embeddings=True)
    qv = np.asarray(qv, dtype=np.float32)

    scores, idxs = RAG_INDEX.search(qv, k=k)

    blocks = []
    total_chars = 0

    for row_idx, score in zip(idxs[0], scores[0]):
        if row_idx < 0:
            continue

        row = RAG_ROWS[int(row_idx)]
        block = (
            f"Source: {row['source']}\n"
            f"Label: {row['label']}\n"
            f"Similarity: {score:.3f}\n"
            f"Statement: {row['text']}"
        )

        if total_chars + len(block) > config["rag_max_chars"]:
            break

        blocks.append(block)
        total_chars += len(block)

    if not blocks:
        return "Retrieved support examples:\n  (no relevant dataset rows retrieved)\n"

    return "Retrieved support examples:\n" + "\n\n---\n\n".join(blocks) + "\n"


####################################################
## BOX 2 — CONTEXT ASSEMBLY
####################################################

def _build_user_message(user_input, profile, past_entries, tips, rag_context):
    """Assemble the user-turn context from all sources."""

    profile_block = (
        f"User profile:\n"
        f"  Name            : {profile['name']}\n"
        f"  Goals           : {', '.join(profile['goals'])}\n"
        f"  Stress triggers : {', '.join(profile['stress_triggers'])}\n"
    )

    if past_entries:
        recent = past_entries[-config["memory_window"]:]
        history_lines = "\n".join(
            f"  - {e['entry']} (felt {e['emotion']})" for e in recent
        )
        history_block = f"Recent journal history:\n{history_lines}\n"
    else:
        history_block = "Recent journal history:\n  (no previous entries)\n"

    tips_block = (
        "Suggested activities for this emotion:\n"
        + "\n".join(f"  {t['category']}: {t['text']}" for t in tips)
        + "\n"
    )

    rag_block = f"{rag_context}\n"
    entry_block = f"New journal entry:\n  {user_input}\n"

    instructions_block = (
        "Task:\n"
        "  1. Classify the user's current emotion.\n"
        "  2. Write one warm, personalised recommendation.\n"
        "  3. Use the retrieved examples only as background context, not as labels to copy.\n"
        "  4. Reference the user's goals or triggers where relevant.\n"
        "  5. If the entry suggests crisis, self-harm, or immediate danger, respond supportively and encourage help-seeking.\n"
    )

    return (
        f"{profile_block}\n"
        f"{history_block}\n"
        f"{tips_block}\n"
        f"{rag_block}"
        f"{entry_block}\n"
        f"{instructions_block}"
    )


####################################################
## BOX 1 — INIT STATE
####################################################

def init_state(user_input, profile, past_entries, tips, rag_context):
    return {
        "status": "IN_PROGRESS",
        "started_at": time.time(),
        "steps": 0,
        "retries": 0,
        "user_input": user_input,
        "tips": tips,
        "rag_context": rag_context,
        "messages": [
            {
                "role": "system",
                "content": (
                    "You are a compassionate mood journal assistant.\n\n"
                    "SAFETY RULE:\n"
                    "If the user expresses suicidal thoughts, self-harm, or immediate danger:\n"
                    "- Set emotion to 'stressed' or 'sad'\n"
                    "- Provide a supportive recommendation encouraging them to seek help now\n"
                    "- Suggest contacting a trusted person, mental health professional, or emergency/crisis resource\n"
                    "- Do not minimize the situation\n\n"
                    "Return ONLY valid JSON with exactly two keys:\n"
                    "  emotion (one of: happy, sad, stressed, neutral)\n"
                    "  recommendation (a warm, personalised string under 60 words)\n"
                    "No extra text. No markdown. Just the JSON object."
                ),
            },
            {
                "role": "user",
                "content": _build_user_message(
                    user_input, profile, past_entries, tips, rag_context
                ),
            },
        ],
        "raw_output": None,
        "validation_result": None,
        "trace": [],
    }


####################################################
## BOX 4 — VALIDATION
####################################################

def validate_output(text):
    clean = (
        text.strip()
        .removeprefix("```json")
        .removeprefix("```")
        .removesuffix("```")
        .strip()
    )
    try:
        obj = json.loads(clean)
    except Exception as e:
        return {"error": f"Not valid JSON: {e}", "parsed": None}

    if not isinstance(obj, dict):
        return {"error": "JSON must be an object.", "parsed": None}

    if "emotion" not in obj or "recommendation" not in obj:
        return {"error": "Missing required keys: emotion, recommendation.", "parsed": None}

    if obj["emotion"].lower() not in VALID_EMOTIONS:
        return {"error": f"emotion must be one of {VALID_EMOTIONS}.", "parsed": None}

    if not isinstance(obj["recommendation"], str) or not obj["recommendation"].strip():
        return {"error": "recommendation must be a non-empty string.", "parsed": None}

    obj["emotion"] = obj["emotion"].lower()
    return {"error": None, "parsed": obj}


####################################################
## BOX 5 — CONTROL
####################################################

def control(state):
    if state["status"] in ("SUCCESS", "FAILED"):
        return True

    if (time.time() - state["started_at"]) > config["timeout_s"]:
        state["status"] = "FAILED"
        state["trace"].append({"event": "control", "reason": "timeout"})
        return True

    if state["steps"] >= config["max_steps"]:
        state["status"] = "FAILED"
        state["trace"].append({"event": "control", "reason": "max_steps"})
        return True

    result = state["validation_result"]

    if result is None:
        return False

    if result["parsed"] is not None:
        state["status"] = "SUCCESS"
        return True

    if state["retries"] >= config["max_retries"]:
        state["status"] = "FAILED"
        state["trace"].append({"event": "control", "reason": "max_retries"})
        return True

    return False


####################################################
## BOX 5 — POLICY
####################################################

def policy(state):
    return "CALL_MODEL"


####################################################
## BOX 5 — TRANSITION
####################################################

def transition(state, action):
    if action == "CALL_MODEL":
        is_repair = (
            state["validation_result"] is not None
            and state["validation_result"]["parsed"] is None
        )

        if not is_repair:
            temperature = config["temperature_first"]
        else:
            temperature = config["temperature_retry"]
            state["retries"] += 1

            prev = state["raw_output"] or ""
            state["messages"].append({"role": "assistant", "content": prev})
            state["messages"].append({
                "role": "system",
                "content": (
                    f"Your previous output was invalid. "
                    f"Reason: {state['validation_result']['error']}\n"
                    f"Return ONLY valid JSON with exactly two keys:\n"
                    f"  emotion (one of: happy, sad, stressed, neutral)\n"
                    f"  recommendation (a warm, personalised string under 60 words)\n"
                    f"No extra text. No markdown. Just the JSON object."
                ),
            })
            state["trace"].append({
                "event": "retry",
                "attempt": state["retries"],
                "reason": state["validation_result"]["error"],
            })

        resp = ollama_chat(
            model=config["model"],
            messages=state["messages"],
            options={
                "temperature": temperature,
                "num_predict": config["num_predict"],
            },
        )

        text = resp["text"]
        state["raw_output"] = text
        state["steps"] += 1
        state["validation_result"] = validate_output(text)

        state["trace"].append({
            "event": "llm_call",
            "step": state["steps"],
            "valid": state["validation_result"]["error"] is None,
        })

    return state


####################################################
## BOX 5 — RUN AGENT
####################################################

def run_agent(user_input, profile, past_entries):
    tips = get_tips("neutral", n=config["tips_per_entry"])
    rag_context = retrieve_rag_context(user_input) if config["use_rag"] else (
        "Retrieved support examples:\n  (RAG disabled in config)\n"
    )

    state = init_state(user_input, profile, past_entries, tips, rag_context)

    while True:
        if control(state):
            break
        action = policy(state)
        state = transition(state, action)

    if state["status"] == "SUCCESS":
        parsed = state["validation_result"]["parsed"]
        final_tips = get_tips(parsed["emotion"], n=config["tips_per_entry"])
        state["tips"] = final_tips

        save_entry(
            entry=user_input,
            emotion=parsed["emotion"],
            recommendation=parsed["recommendation"],
            tips=[t["text"] for t in final_tips],
        )

    return state


####################################################
## BOX 6 — TERMINAL DASHBOARD (quick summary)
####################################################

def print_summary(state):
    parsed = state["validation_result"]["parsed"]
    tips = state["tips"]
    emotion = parsed["emotion"]
    rec = parsed["recommendation"]

    print(f"\n  Emotion        : {emotion}")
    print(f"  Recommendation : {rec}")
    print(f"\n  Things to try:")
    for t in tips:
        print(f"    [{t['category']}] {t['text']}")
    print("\n  Is there anything else you want to share?")
    print()


def wants_to_continue():
    """
    Ask the user whether they want to keep chatting after each response.
    """
    while True:
        answer = input(config["follow_up_prompt"]).strip().lower()

        if answer in ("yes", "y", "sure", "ok", "okay"):
            print()
            return True

        if answer in ("no", "n", "nope"):
            print("\nThanks for sharing today.")
            return False

        print("  Please answer yes or no.\n")


def generate_dashboard():
    """
    Generate the HTML dashboard automatically when the chat ends.
    """
    import webbrowser

    journal = load_journal()
    profile = load_profile()

    if not journal:
        print("No journal entries yet, so no dashboard was generated.\n")
        return

    html = build_html(journal, profile)
    out_file = "mood_dashboard.html"

    with open(out_file, "w", encoding="utf-8") as f:
        f.write(html)

    abs_path = os.path.abspath(out_file)
    print(f"Dashboard saved to {out_file}")

    try:
        webbrowser.open(f"file://{abs_path}")
    except Exception:
        pass

    print()


####################################################
## BOX 1 — USER INTERACTION LOOP
####################################################

if __name__ == "__main__":
    if config["use_rag"]:
        init_rag()
        print(RAG_STATUS)

    profile = load_profile()
    if profile is None:
        profile = setup_profile()
    else:
        print(f"\nWelcome back, {profile['name']}!\n")

    print("Mood Journal — type your entry, or 'quit' to exit.\n")

    while True:
        user_input = input("Journal entry: ").strip()

        if user_input.lower() in ("quit", "exit", "q"):
            print()
            generate_dashboard()
            break

        if not user_input:
            print("  Please write something.\n")
            continue

        past_entries = load_journal()
        state = run_agent(user_input, profile, past_entries)

        if state["status"] == "SUCCESS":
            print_summary(state)
            if not wants_to_continue():
                generate_dashboard()
                break
        else:
            print("\n  Could not process your entry. Please try again.\n")
