import { useState, useRef, useCallback, useEffect } from "react";
import type { WorkerResponse } from "../workers/whisper.worker";

export type WhisperStatus =
  | "idle" // No activity
  | "loading" // Model downloading/loading
  | "ready" // Model loaded, ready to record
  | "recording" // Microphone active, capturing audio
  | "transcribing"; // Audio captured, Whisper processing

export interface WhisperProgress {
  /** File currently being downloaded */
  file?: string;
  /** Download progress 0-100 */
  percent?: number;
}

export interface UseWhisperReturn {
  status: WhisperStatus;
  progress: WhisperProgress;
  /** Error message if something went wrong */
  error: string | null;
  /** Start recording. Loads model on first call. */
  startRecording: () => Promise<void>;
  /** Stop recording and begin transcription. */
  stopRecording: () => void;
  /** Whether the browser supports the required APIs */
  isSupported: boolean;
}

const SAMPLE_RATE = 16_000; // Whisper expects 16kHz audio

export function useWhisper(
  onTranscription: (text: string) => void,
): UseWhisperReturn {
  const [status, setStatus] = useState<WhisperStatus>("idle");
  const [progress, setProgress] = useState<WhisperProgress>({});
  const [error, setError] = useState<string | null>(null);

  const workerRef = useRef<Worker | null>(null);
  const modelLoadedRef = useRef(false);
  const modelLoadingPromiseRef = useRef<Promise<void> | null>(null);

  // Audio recording refs
  const mediaStreamRef = useRef<MediaStream | null>(null);
  const audioContextRef = useRef<AudioContext | null>(null);
  const sourceRef = useRef<MediaStreamAudioSourceNode | null>(null);
  const processorRef = useRef<ScriptProcessorNode | null>(null);
  const audioChunksRef = useRef<Float32Array[]>([]);
  const nativeSampleRateRef = useRef<number>(SAMPLE_RATE);

  const isSupported =
    typeof window !== "undefined" &&
    !!navigator.mediaDevices?.getUserMedia &&
    !!window.AudioContext;

  /** Get or create the worker singleton */
  const getWorker = useCallback(() => {
    if (!workerRef.current) {
      workerRef.current = new Worker(
        new URL("../workers/whisper.worker.ts", import.meta.url),
        { type: "module" },
      );
    }
    return workerRef.current;
  }, []);

  /** Ensure the model is loaded. Returns immediately if already loaded. */
  const ensureModelLoaded = useCallback((): Promise<void> => {
    if (modelLoadedRef.current) {
      return Promise.resolve();
    }
    if (modelLoadingPromiseRef.current) {
      return modelLoadingPromiseRef.current;
    }

    const loadPromise = new Promise<void>((resolve, reject) => {
      const worker = getWorker();

      const handleMessage = (event: MessageEvent<WorkerResponse>) => {
        const msg = event.data;
        switch (msg.type) {
          case "loading":
            setStatus("loading");
            break;
          case "progress": {
            const info = msg.progress;
            if (info.status === "progress") {
              setProgress({
                file: (info as { file?: string }).file,
                percent: (info as { progress?: number }).progress,
              });
            }
            break;
          }
          case "ready":
            modelLoadedRef.current = true;
            setStatus("ready");
            setProgress({});
            worker.removeEventListener("message", handleMessage);
            resolve();
            break;
          case "error":
            setError(msg.error);
            setStatus("idle");
            worker.removeEventListener("message", handleMessage);
            modelLoadingPromiseRef.current = null;
            reject(new Error(msg.error));
            break;
        }
      };

      worker.addEventListener("message", handleMessage);
      worker.postMessage({ type: "load" });
    });

    modelLoadingPromiseRef.current = loadPromise;
    return loadPromise;
  }, [getWorker]);

  /** Clean up audio recording resources */
  const cleanupRecording = useCallback(() => {
    processorRef.current?.disconnect();
    sourceRef.current?.disconnect();
    audioContextRef.current?.close();
    mediaStreamRef.current?.getTracks().forEach((track) => track.stop());
    processorRef.current = null;
    sourceRef.current = null;
    audioContextRef.current = null;
    mediaStreamRef.current = null;
  }, []);

  /** Start recording audio from the microphone */
  const startRecording = useCallback(async () => {
    if (!isSupported) {
      setError("Browser does not support audio recording");
      return;
    }

    setError(null);

    // Ensure model is loaded first (shows loading progress on first call)
    try {
      await ensureModelLoaded();
    } catch {
      // Error already set by ensureModelLoaded
      return;
    }

    // Request microphone access
    try {
      const stream = await navigator.mediaDevices.getUserMedia({
        audio: {
          channelCount: 1,
        },
      });

      mediaStreamRef.current = stream;

      // Use the browser's native sample rate to avoid cross-rate errors,
      // then resample to 16kHz before sending to Whisper
      const audioContext = new AudioContext();
      audioContextRef.current = audioContext;
      nativeSampleRateRef.current = audioContext.sampleRate;

      const source = audioContext.createMediaStreamSource(stream);
      sourceRef.current = source;

      // Use ScriptProcessorNode to capture raw PCM samples at native rate
      const processor = audioContext.createScriptProcessor(4096, 1, 1);
      processorRef.current = processor;

      audioChunksRef.current = [];

      processor.onaudioprocess = (event) => {
        const channelData = event.inputBuffer.getChannelData(0);
        audioChunksRef.current.push(new Float32Array(channelData));
      };

      source.connect(processor);
      processor.connect(audioContext.destination);

      setStatus("recording");
    } catch (err) {
      const message =
        err instanceof Error ? err.message : "Microphone access denied";
      setError(message);
      setStatus(modelLoadedRef.current ? "ready" : "idle");
    }
  }, [isSupported, ensureModelLoaded]);

  /** Resample audio from the native sample rate to Whisper's 16kHz using OfflineAudioContext */
  const resampleTo16kHz = useCallback(
    async (
      inputData: Float32Array,
      inputSampleRate: number,
    ): Promise<Float32Array> => {
      // If already at 16kHz, no resampling needed
      if (inputSampleRate === SAMPLE_RATE) {
        return inputData;
      }

      const durationSeconds = inputData.length / inputSampleRate;
      const outputLength = Math.ceil(durationSeconds * SAMPLE_RATE);

      const offlineContext = new OfflineAudioContext(
        1,
        outputLength,
        SAMPLE_RATE,
      );
      const buffer = offlineContext.createBuffer(
        1,
        inputData.length,
        inputSampleRate,
      );
      buffer.getChannelData(0).set(inputData);

      const source = offlineContext.createBufferSource();
      source.buffer = buffer;
      source.connect(offlineContext.destination);
      source.start(0);

      const renderedBuffer = await offlineContext.startRendering();
      return renderedBuffer.getChannelData(0);
    },
    [],
  );

  /** Stop recording and send audio to Whisper for transcription */
  const stopRecording = useCallback(() => {
    if (status !== "recording") return;

    const nativeSampleRate = nativeSampleRateRef.current;

    // Stop capturing
    cleanupRecording();

    // Merge all audio chunks into a single Float32Array
    const chunks = audioChunksRef.current;
    const totalLength = chunks.reduce((sum, chunk) => sum + chunk.length, 0);

    if (totalLength === 0) {
      setStatus("ready");
      return;
    }

    const rawAudio = new Float32Array(totalLength);
    let offset = 0;
    for (const chunk of chunks) {
      rawAudio.set(chunk, offset);
      offset += chunk.length;
    }
    audioChunksRef.current = [];

    // Resample and send to worker
    setStatus("transcribing");

    const worker = getWorker();

    const handleMessage = (event: MessageEvent<WorkerResponse>) => {
      const msg = event.data;
      switch (msg.type) {
        case "complete":
          setStatus("ready");
          worker.removeEventListener("message", handleMessage);
          if (msg.text) {
            onTranscription(msg.text);
          }
          break;
        case "error":
          setError(msg.error);
          setStatus("ready");
          worker.removeEventListener("message", handleMessage);
          break;
      }
    };

    worker.addEventListener("message", handleMessage);

    // Resample from native rate (e.g. 44100/48000) to 16kHz, then send
    resampleTo16kHz(rawAudio, nativeSampleRate)
      .then((audioData) => {
        worker.postMessage({ type: "transcribe", audio: audioData });
      })
      .catch((err) => {
        const message =
          err instanceof Error ? err.message : "Resampling failed";
        setError(message);
        setStatus("ready");
        worker.removeEventListener("message", handleMessage);
      });
  }, [status, cleanupRecording, getWorker, onTranscription, resampleTo16kHz]);

  // Cleanup on unmount
  useEffect(() => {
    return () => {
      cleanupRecording();
      workerRef.current?.terminate();
      workerRef.current = null;
    };
  }, [cleanupRecording]);

  return {
    status,
    progress,
    error,
    startRecording,
    stopRecording,
    isSupported,
  };
}
