test: test-rust test-tree-sitter test-wasm test-node

test-rust: 
  cargo nextest run --all-features

test-node:
  cd {{justfile_directory()}}/jexl-node && \
    npm run build && \
    npm run test

preview: build-wasm build-tree-sitter build-jexl-3000-monaco
  cd {{justfile_directory()}}/demo-monaco && \
    npm run preview

build-wasm:
  wasm-pack build {{justfile_directory()}}/jexl-wasm \
    --target web --release \
    --no-default-features --features language-service && \
        wasm-opt --inlining-optimizing -Oz -o {{justfile_directory()}}/jexl-wasm/pkg/jexl_wasm_bg.wasm {{justfile_directory()}}/jexl-wasm/pkg/jexl_wasm_bg.wasm && \
        node jexl-3000-monaco/scripts/copy-assets.js && \
        node demo-monaco/scripts/copy-assets.js

twiggy-wasm:
  RUSTFLAGS='-C debuginfo=2' wasm-pack build {{justfile_directory()}}/jexl-wasm \
    --target web --dev \
    --no-default-features --features language-service && \
        twiggy top -n 200 {{justfile_directory()}}/jexl-wasm/pkg/jexl_wasm_bg.wasm

build-tree-sitter:
  cd {{justfile_directory()}}/tree-sitter-jexl3000 && \
    tree-sitter generate && \
    tree-sitter build -w --reuse-allocator && \
        cd {{justfile_directory()}} && \
        node jexl-3000-monaco/scripts/copy-assets.js && \
        node demo-monaco/scripts/copy-assets.js

test-tree-sitter:
  cd {{justfile_directory()}}/tree-sitter-jexl3000 && \
    tree-sitter generate && \
    tree-sitter build && \
    tree-sitter test --rebuild

build-parser:
  cargo run --bin parser-gen

build-napi:
  cd {{justfile_directory()}}/jexl-node && RUSTFLAGS="-C target-cpu=native" npm run build

build-jexl-3000-monaco:
  cd {{justfile_directory()}}/jexl-3000-monaco && npm run build && \
    cd {{justfile_directory()}}/demo-monaco && npm run prepare-assets

prepare-assets-demo-monaco:
  cd {{justfile_directory()}}/demo-monaco && npm run prepare-assets

test-wasm:
  cd {{justfile_directory()}}/jexl-wasm && wasm-pack test --node
  cd {{justfile_directory()}}/jexl-wasm && wasm-pack test --node -- --features language-service

publish:
  cd {{justfile_directory()}}/jexl-serverless && wrangler publish

deploy: test build-wasm build-tree-sitter build-jexl-3000-monaco
  cd {{justfile_directory()}}/demo-monaco && npm run build && netlify deploy

deploy-prod: test build-wasm build-tree-sitter build-jexl-3000-monaco
  cd {{justfile_directory()}}/demo-monaco && npm run build && netlify deploy --prod
