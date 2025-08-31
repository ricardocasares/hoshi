import { defineConfig } from "vite";
import css from "@tailwindcss/vite";
import elm from "vite-plugin-elm";
import paths from "vite-tsconfig-paths";

export default defineConfig({
  plugins: [paths(), css(), elm()],
});
