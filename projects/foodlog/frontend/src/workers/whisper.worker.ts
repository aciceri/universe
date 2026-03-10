import {
  pipeline,
  env,
  type AutomaticSpeechRecognitionPipeline,
  type ProgressInfo,
} from "@huggingface/transformers";

// Skip local model check — always download from Hugging Face CDN
env.allowLocalModels = false;

// Singleton: loaded once, reused across transcription requests
let transcriber: AutomaticSpeechRecognitionPipeline | null = null;

// Message types for communication with the main thread
export type WorkerRequest =
  | { type: "load" }
  | { type: "transcribe"; audio: Float32Array };

export type WorkerResponse =
  | { type: "loading" }
  | { type: "progress"; progress: ProgressInfo }
  | { type: "ready" }
  | { type: "transcribing" }
  | { type: "complete"; text: string; timeMs: number }
  | { type: "error"; error: string };

function post(msg: WorkerResponse) {
  self.postMessage(msg);
}

async function loadModel() {
  post({ type: "loading" });

  // @ts-expect-error — pipeline() overload union is too complex for TS to resolve
  transcriber = await pipeline(
    "automatic-speech-recognition",
    "onnx-community/whisper-small-ita-ONNX",
    {
      dtype: "q4",
      device: "wasm",
      progress_callback: (progress: ProgressInfo) => {
        post({ type: "progress", progress });
      },
    },
  );

  post({ type: "ready" });
}

async function transcribe(audio: Float32Array) {
  if (!transcriber) {
    post({ type: "error", error: "Model not loaded yet" });
    return;
  }

  post({ type: "transcribing" });

  const startTime = performance.now();

  const result = await transcriber(audio, {
    language: "italian",
    task: "transcribe",
    chunk_length_s: 30,
    stride_length_s: 5,
  });

  const timeMs = performance.now() - startTime;

  // Result can be a single object or array; normalize to single text
  const text = Array.isArray(result)
    ? result.map((r) => r.text).join(" ")
    : result.text;

  post({ type: "complete", text: text.trim(), timeMs });
}

self.addEventListener("message", async (event: MessageEvent<WorkerRequest>) => {
  try {
    switch (event.data.type) {
      case "load":
        await loadModel();
        break;
      case "transcribe":
        await transcribe(event.data.audio);
        break;
    }
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    post({ type: "error", error: message });
  }
});
