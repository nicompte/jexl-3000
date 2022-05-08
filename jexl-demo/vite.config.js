import { defineConfig } from 'vite';
import wasmPack from 'vite-plugin-wasm-pack';

export default defineConfig({
    // pass your local crate path to the plugin
    plugins: [wasmPack('../jexl-wasm')],
    build: {
        target: ['chrome95', 'firefox95']
    },
});