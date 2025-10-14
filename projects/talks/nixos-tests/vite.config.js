import { defineConfig } from "vite";
import { copyFileSync } from "fs";

export default defineConfig({
  base: "./",
  build: {
    rollupOptions: {
      input: {
        main: "index.html",
      },
    },
  },
  plugins: [
    {
      name: "copy-slides",
      writeBundle() {
        copyFileSync("slides.md", "dist/slides.md");
      },
    },
  ],
});
