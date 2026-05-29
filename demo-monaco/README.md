## jexl-demo

Source for [https://jexl-3000.barbotte.net](https://jexl-3000.barbotte.net).

Notes for deployment

- Vendored assets (wasm and monaco files) are stored in public/assets/jexl3000/ and are committed to the repo to avoid building wasm in CI.
- To refresh vendored assets locally (only necessary when rebuilding jexl-wasm or jexl-3000-monaco): run `npm run prepare-assets` from the demo-monaco directory, or run `just prepare-assets-demo-monaco` from the repo root.

Deployment (platform specifics)

Netlify
- Base directory: demo-monaco
- Build command: npm ci && npm run build
- Publish directory: dist
- A root-level netlify.toml is included to set these values automatically.

Vercel
- Project root: demo-monaco
- Build command: npm ci && npm run build
- Output directory: dist
- A vercel.json is included to instruct Vercel to build the demo-monaco package and use dist as the publish directory.

Cloudflare Pages
- Base directory: demo-monaco
- Build command: npm ci && npm run build
- Build output directory: dist

Note: Because the required wasm and Monaco assets are vendored in public/assets/jexl3000/, platform builds do not need Rust or wasm-pack; CI can skip wasm-related steps.
