
import init, { Evaluator } from 'jexl-wasm';
import { Sitter as TreeSitter } from '../public/tree-sitter';

let tree;
let evaluator;

(async () => {
  const CAPTURE_REGEX = /@\s*([\w\._-]+)/g;
  const COLORS_BY_INDEX = [
    'blue',
    'chocolate',
    'darkblue',
    'darkcyan',
    'darkgreen',
    'darkred',
    'darkslategray',
    'dimgray',
    'green',
    'indigo',
    'navy',
    'red',
    'sienna',
  ];

  // const scriptURL = document.currentScript.getAttribute('src');

  const codeInput = document.getElementById('code-input');
  const languageSelect = document.getElementById('language-select');
  const loggingCheckbox = document.getElementById('logging-checkbox');
  const outputContainer = document.getElementById('output-container');
  const outputContainerScroll = document.getElementById('output-container-scroll');
  const playgroundContainer = document.getElementById('playground-container');
  const queryCheckbox = document.getElementById('query-checkbox');
  const queryContainer = document.getElementById('query-container');
  const queryInput = document.getElementById('query-input');
  const contextInput = document.getElementById('context-input');
  const responseInput = document.getElementById('response-input');

  const updateTimeSpan = document.getElementById('update-time');
  const languagesByName = {};

  await loadState();

  await TreeSitter.init();

  const parser = new TreeSitter();
  const codeEditor = CodeMirror.fromTextArea(codeInput, {
    lineNumbers: true,
    showCursorWhenSelecting: true,
    tabSize: 2,
  });

  const queryEditor = CodeMirror.fromTextArea(queryInput, {
    lineNumbers: true,
    showCursorWhenSelecting: true,
    tabSize: 2,
  });

  const contextEditor = CodeMirror.fromTextArea(contextInput, {
    lineNumbers: true,
    showCursorWhenSelecting: true,
    mode: 'javascript',
    tabSize: 2,
  });

  const responseEditor = CodeMirror.fromTextArea(responseInput, {
    lineNumbers: true,
    showCursorWhenSelecting: true,
    readOnly: true,
    mode: 'javascript',
    lineWrapping: true,
    tabSize: 2,
  });

  const cluster = new Clusterize({
    rows: [],
    noDataText: null,
    contentElem: outputContainer,
    scrollElem: outputContainerScroll
  });
  const renderTreeOnCodeChange = debounce(renderTree, 50);
  const saveStateOnChange = debounce(saveState, 2000);
  const runTreeQueryOnChange = debounce(runTreeQuery, 50);

  let languageName = languageSelect.value;
  let parseCount = 0;
  let query;

  codeEditor.on('changes', debounce(handleCodeChange, 100));
  codeEditor.on('viewportChange', runTreeQueryOnChange);
  queryEditor.on('changes', debounce(handleQueryChange, 100));
  contextEditor.on('changes', debounce(handleContextChange, 100));

  loggingCheckbox.addEventListener('change', handleLoggingChange);
  queryCheckbox.addEventListener('change', handleQueryEnableChange);
  languageSelect.addEventListener('change', handleLanguageChange);

  handleQueryEnableChange();
  await handleLanguageChange()

  playgroundContainer.style.visibility = 'visible';

  async function handleContextChange() {
    const newContext = contextEditor.getValue();
    localStorage.setItem('context', newContext);
    if (evaluator === undefined) {
      let formData = new FormData();
      formData.append('context', newContext);
      formData.append('expression', codeEditor.getValue());
      const req = await fetch(
        'https://jexl-serverless.barbotte.workers.dev/evaluate',
        {
          method: 'POST',
          body: formData,
        });
      const res = await req.json();
      if (res.result) {
        responseEditor.setValue(JSON.stringify(res.result, null, 2));
      } else {
        localStorage.clear();
        location.reload();
      }
      init().then(() => {
        evaluator = new Evaluator();
      });
    } else {
      try {
        const now = performance.now();
        const res = evaluator.evaluate(codeEditor.getValue(), JSON.parse(newContext));
        responseEditor.setValue(JSON.stringify(res, null, 2));
        console.log(`Evaluation took ${performance.now() - now}ms`);
      } catch (e) {
        responseEditor.setValue(JSON.stringify(e, null, 2));
      }
    }
  }

  async function handleLanguageChange() {
    const newLanguageName = languageSelect.value;
    if (!languagesByName[newLanguageName]) {
      const url = `${LANGUAGE_BASE_URL}/tree-sitter-${newLanguageName}.wasm`
      languageSelect.disabled = true;
      try {
        languagesByName[newLanguageName] = await TreeSitter.Language.load(url);
      } catch (e) {
        console.error(e);
        languageSelect.value = languageName;
        return
      } finally {
        languageSelect.disabled = false;
      }
    }

    tree = null;
    languageName = newLanguageName;
    parser.setLanguage(languagesByName[newLanguageName]);
    handleCodeChange();
    handleQueryChange();
  }

  async function handleCodeChange(editor, changes) {
    const newText = codeEditor.getValue() + '\n';
    const edits = tree && changes && changes.map(treeEditForEditorChange);

    const start = performance.now();
    if (edits) {
      for (const edit of edits) {
        tree.edit(edit);
      }
    }
    const newTree = parser.parse(newText, tree);
    const duration = (performance.now() - start).toFixed(1);

    updateTimeSpan.innerText = `${duration} ms`;
    if (tree) tree.delete();
    tree = newTree;
    parseCount++;
    renderTreeOnCodeChange();
    runTreeQueryOnChange();
    saveStateOnChange();
    handleContextChange();
  }

  async function renderTree() {
    const cursor = tree.walk();

    let currentRenderCount = parseCount;
    let row = '';
    let rows = [];
    let finishedRow = false;
    let visitedChildren = false;
    let indentLevel = 0;

    for (let i = 0; ; i++) {
      if (i > 0 && i % 10000 === 0) {
        await new Promise(r => setTimeout(r, 0));
        if (parseCount !== currentRenderCount) {
          cursor.delete();
          return;
        }
      }

      let displayName;
      if (cursor.nodeIsMissing) {
        displayName = `MISSING ${cursor.nodeType}`
      } else if (cursor.nodeIsNamed) {
        displayName = cursor.nodeType;
      }

      if (visitedChildren) {
        if (displayName) {
          finishedRow = true;
        }

        if (cursor.gotoNextSibling()) {
          visitedChildren = false;
        } else if (cursor.gotoParent()) {
          visitedChildren = true;
          indentLevel--;
        } else {
          break;
        }
      } else {
        if (displayName) {
          if (finishedRow) {
            row += '</div>';
            rows.push(row);
            finishedRow = false;
          }
          const start = cursor.startPosition;
          const end = cursor.endPosition;
          const id = cursor.nodeId;
          let fieldName = cursor.currentFieldName();
          if (fieldName) {
            fieldName += ': ';
          } else {
            fieldName = '';
          }
          row = `<div>${'  '.repeat(indentLevel)}${fieldName}<a class='plain' href="#" data-id=${id} data-range="${start.row},${start.column},${end.row},${end.column}">${displayName}</a> [${start.row}, ${start.column}] - [${end.row}, ${end.column}]`;
          finishedRow = true;
        }

        if (cursor.gotoFirstChild()) {
          visitedChildren = false;
          indentLevel++;
        } else {
          visitedChildren = true;
        }
      }
    }
    if (finishedRow) {
      row += '</div>';
      rows.push(row);
    }

    cursor.delete();
    cluster.update(rows);
    // treeRows = rows;
  }

  function runTreeQuery(_, startRow, endRow) {
    if (endRow == null) {
      const viewport = codeEditor.getViewport();
      startRow = viewport.from;
      endRow = viewport.to;
    }

    codeEditor.operation(() => {
      const marks = codeEditor.getAllMarks();
      marks.forEach(m => m.clear());

      if (tree && query) {
        const captures = query.captures(
          tree.rootNode,
          { row: startRow, column: 0 },
          { row: endRow, column: 0 },
        );
        let lastNodeId;
        for (const { name, node } of captures) {
          if (node.id === lastNodeId) continue;
          lastNodeId = node.id;
          const { startPosition, endPosition } = node;
          codeEditor.markText(
            { line: startPosition.row, ch: startPosition.column },
            { line: endPosition.row, ch: endPosition.column },
            {
              inclusiveLeft: true,
              inclusiveRight: true,
              css: `color: ${colorForCaptureName(name)}`
            }
          );
        }
      }
    });
  }

  function handleQueryChange() {
    if (query) {
      query.delete();
      query.deleted = true;
      query = null;
    }

    queryEditor.operation(() => {
      queryEditor.getAllMarks().forEach(m => m.clear());
      if (!queryCheckbox.checked) return;

      const queryText = queryEditor.getValue();

      try {
        query = parser.getLanguage().query(queryText);
        let match;

        let row = 0;
        queryEditor.eachLine((line) => {
          while (match = CAPTURE_REGEX.exec(line.text)) {
            queryEditor.markText(
              { line: row, ch: match.index },
              { line: row, ch: match.index + match[0].length },
              {
                inclusiveLeft: true,
                inclusiveRight: true,
                css: `color: ${colorForCaptureName(match[1])}`
              }
            );
          }
          row++;
        });
      } catch (error) {
        const startPosition = queryEditor.posFromIndex(error.index);
        const endPosition = {
          line: startPosition.line,
          ch: startPosition.ch + (error.length || Infinity)
        };

        if (error.index === queryText.length) {
          if (startPosition.ch > 0) {
            startPosition.ch--;
          } else if (startPosition.row > 0) {
            startPosition.row--;
            startPosition.column = Infinity;
          }
        }

        queryEditor.markText(
          startPosition,
          endPosition,
          {
            className: 'query-error',
            inclusiveLeft: true,
            inclusiveRight: true,
            attributes: { title: error.message }
          }
        );
      }
    });

    runTreeQuery();
    saveQueryState();
  }

  function handleLoggingChange() {
    if (loggingCheckbox.checked) {
      parser.setLogger((message, lexing) => {
        if (lexing) {
          console.log("  ", message)
        } else {
          console.log(message)
        }
      });
    } else {
      parser.setLogger(null);
    }
  }

  function handleQueryEnableChange() {
    if (queryCheckbox.checked) {
      queryContainer.style.visibility = '';
      queryContainer.style.position = '';
    } else {
      queryContainer.style.visibility = 'hidden';
      queryContainer.style.position = 'absolute';
    }
    handleQueryChange();
  }

  function treeEditForEditorChange(change) {
    const oldLineCount = change.removed.length;
    const newLineCount = change.text.length;
    const lastLineLength = change.text[newLineCount - 1].length;

    const startPosition = { row: change.from.line, column: change.from.ch };
    const oldEndPosition = { row: change.to.line, column: change.to.ch };
    const newEndPosition = {
      row: startPosition.row + newLineCount - 1,
      column: newLineCount === 1
        ? startPosition.column + lastLineLength
        : lastLineLength
    };

    const startIndex = codeEditor.indexFromPos(change.from);
    let newEndIndex = startIndex + newLineCount - 1;
    let oldEndIndex = startIndex + oldLineCount - 1;
    for (let i = 0; i < newLineCount; i++) newEndIndex += change.text[i].length;
    for (let i = 0; i < oldLineCount; i++) oldEndIndex += change.removed[i].length;

    return {
      startIndex, oldEndIndex, newEndIndex,
      startPosition, oldEndPosition, newEndPosition
    };
  }

  function colorForCaptureName(capture) {
    const id = query.captureNames.indexOf(capture);
    return COLORS_BY_INDEX[id % COLORS_BY_INDEX.length];
  }

  async function loadState() {
    if (!localStorage.getItem("sourceCode")) {
      const req = await fetch('/default_expression.jexl-3000');
      const res = await req.text();
      localStorage.setItem('sourceCode', res);
    }
    if (!localStorage.getItem("query")) {
      const req = await fetch('/highlights.scm');
      const res = await req.text();
      localStorage.setItem('query', res);
      localStorage.setItem("queryEnabled", true);

    }
    if (!localStorage.getItem("context")) {
      const req = await fetch('/default_context.json');
      const res = await req.text();
      localStorage.setItem('context', res);
    }
    const language = 'jexl3000';
    const sourceCode = localStorage.getItem("sourceCode");
    const query = localStorage.getItem("query");
    const context = localStorage.getItem("context");
    const queryEnabled = localStorage.getItem("queryEnabled");
    if (language != null && sourceCode != null && query != null) {
      contextInput.value = context;
      queryInput.value = query;
      codeInput.value = sourceCode;
      languageSelect.value = language;
      queryCheckbox.checked = (queryEnabled === 'true');
    }
  }

  function saveState() {
    localStorage.setItem("language", languageSelect.value);
    localStorage.setItem("sourceCode", codeEditor.getValue());
    saveQueryState();
  }

  function saveQueryState() {
    localStorage.setItem("queryEnabled", queryCheckbox.checked);
    localStorage.setItem("query", queryEditor.getValue());
  }

  function debounce(func, timeout = 100) {
    let timer;
    return (...args) => {
      clearTimeout(timer);
      timer = setTimeout(() => { func.apply(this, args); }, timeout);
    };
  }
})();
