/**
 * ladsubrdisplib.js - Ladder Subroutine Display Library
 * Handles rendering ladder diagram rungs from program data
 *
 * Supports both legacy format and Python-compatible matrixdata format.
 * The matrixdata format includes explicit branch connector cells,
 * eliminating the need for complex branch computation.
 */

const SubrDispControl = (function() {
    'use strict';

    // Current program data
    let currentProgram = null;
    let currentAddresses = [];

    // Cell dimensions for layout
    const CELL_WIDTH = 80;
    const CELL_HEIGHT = 60;
    const BLOCK_CELL_WIDTH = 140;
    const BLOCK_CELL_HEIGHT = 80;

    /**
     * Set the current program data
     * @param {Object} programData - Program data from API
     */
    function setProgram(programData) {
        currentProgram = programData;
        currentAddresses = [];

        if (programData && programData.addresses) {
            currentAddresses = programData.addresses.slice();
        }

        // Collect addresses from rungs
        if (programData && programData.subrdata) {
            programData.subrdata.forEach(rung => {
                // Handle both legacy (cells) and new (matrixdata) formats
                const cells = rung.matrixdata || rung.cells || [];
                cells.forEach(cell => {
                    // New format: addr is an array
                    if (cell.addr && Array.isArray(cell.addr)) {
                        cell.addr.forEach(addr => {
                            if (addr && !currentAddresses.includes(addr)) {
                                currentAddresses.push(addr);
                            }
                        });
                    }
                    // Legacy format: separate addr and addrs
                    if (cell.addrs && Array.isArray(cell.addrs)) {
                        cell.addrs.forEach(addr => {
                            if (addr && !currentAddresses.includes(addr)) {
                                currentAddresses.push(addr);
                            }
                        });
                    }
                    if (cell.addr && typeof cell.addr === 'string' && !currentAddresses.includes(cell.addr)) {
                        currentAddresses.push(cell.addr);
                    }
                });
            });
        }
    }

    /**
     * Get all addresses that need monitoring
     * @returns {string[]} - Array of address strings
     */
    function getMonitorAddresses() {
        return currentAddresses;
    }

    /**
     * Check if symbol represents a block instruction
     * @param {string} symbol - Symbol name
     * @returns {boolean}
     */
    function isBlockSymbol(symbol) {
        return LadSymbols.isBlockSymbol(symbol);
    }

    /**
     * Normalize cell data to a common format
     * Handles both legacy and Python matrixdata formats
     * @param {Object} cell - Cell data in either format
     * @returns {Object} - Normalized cell data
     */
    function normalizeCell(cell) {
        // Determine symbol name (new format uses 'value', legacy uses 'symbol')
        const symbol = cell.value || cell.symbol || 'il';

        // Determine addresses (new format: addr is array, legacy: addrs array + addr string)
        let addresses = [];
        if (cell.addr && Array.isArray(cell.addr)) {
            addresses = cell.addr;
        } else if (cell.addrs && Array.isArray(cell.addrs)) {
            addresses = cell.addrs;
        } else if (cell.addr && typeof cell.addr === 'string') {
            addresses = [cell.addr];
        }

        // Determine cell type
        let cellType = cell.type || 'unknown';
        if (cellType === 'inp') cellType = 'input';
        if (cellType === 'outp') cellType = 'output';

        // Check if this is a branch connector
        const isBranch = LadSymbols.isBranchSymbol(symbol);

        return {
            symbol,
            addresses,
            type: cellType,
            row: cell.row || 0,
            col: cell.col || 0,
            opcode: cell.opcode || '',
            params: cell.params || [],
            monitor: cell.monitor || null,
            isBranch,
            isBlock: isBlockSymbol(symbol),
            isVertical: LadSymbols.isVerticalBranchSymbol(symbol)
        };
    }

    /**
     * Create HTML for a single cell
     * @param {Object} cell - Normalized cell data
     * @param {boolean} forceBlockWidth - Force block width
     * @returns {string} - HTML string
     */
    function createCellHtml(cell, forceBlockWidth = false) {
        const { symbol, addresses, type, row, col, opcode, params, isBranch, isBlock, isVertical } = cell;

        // Create unique ID for monitoring
        const cellId = `cell-${row}-${col}`;

        // Determine cell class
        let cellClass = 'ladder-cell';
        if (isBlock || forceBlockWidth) {
            cellClass += ' ladder-block-cell';
        }
        if (isBranch) {
            cellClass += ' ladder-branch-cell';
        }
        if (isVertical) {
            cellClass += ' ladder-vline-cell';
        }

        // Get SVG symbol
        const svgHtml = LadSymbols.getSymbol(symbol, 'MB_ladderoff');

        // Create address display (not for branch connectors)
        let addressDisplay = '';
        if (!isBranch && addresses.length > 0) {
            addressDisplay = `<span class="cell-address">${escapeHtml(addresses[0])}</span>`;
        }

        // For block types, show parameters
        let paramsDisplay = '';
        if (isBlock && params.length > 0) {
            const displayParams = params.slice(0, 4).join(' ');
            paramsDisplay = `<span class="block-params">${escapeHtml(displayParams)}</span>`;
        }

        // Data attributes for monitoring
        const dataAttrs = addresses.length > 0
            ? `data-addresses="${escapeHtml(addresses.join(','))}"`
            : '';

        return `<div id="${cellId}" class="${cellClass} MB_ladderoff" ${dataAttrs} data-opcode="${escapeHtml(opcode)}" data-symbol="${escapeHtml(symbol)}" data-row="${row}" data-col="${col}"><div class="cell-symbol">${svgHtml}</div>${paramsDisplay}${addressDisplay}<span class="cell-value"></span></div>`;
    }

    /**
     * Create an empty placeholder cell
     * @param {number} row - Row index
     * @param {number} col - Column index
     * @param {boolean} isBlockWidth - If true, use block cell width
     * @returns {string} - HTML string
     */
    function createEmptyHtml(row, col, isBlockWidth = false) {
        const widthClass = isBlockWidth ? 'ladder-empty ladder-empty-block' : 'ladder-empty';
        return `<div class="${widthClass}" data-row="${row}" data-col="${col}"></div>`;
    }

    /**
     * Detect format and check if this is Python-compatible matrixdata
     * @param {Object} rung - Rung data
     * @returns {boolean} - True if using new matrixdata format
     */
    function isMatrixdataFormat(rung) {
        return rung.matrixdata && Array.isArray(rung.matrixdata);
    }

    /**
     * Create HTML for a rung using Python-compatible matrixdata format
     * This is much simpler since branch connectors are explicit cells.
     * @param {Object} rung - Rung data with matrixdata
     * @returns {string} - HTML string
     */
    function createRungHtmlMatrixdata(rung) {
        const rungNum = rung.rungnum || 0;
        const comment = rung.comment || '';
        const matrixdata = rung.matrixdata || [];

        // Separate inputs and outputs
        const inputs = matrixdata.filter(c => c.type === 'inp');
        const outputs = matrixdata.filter(c => c.type === 'outp');

        // Determine grid dimensions from inputs
        let maxRow = 0;
        let maxCol = 0;
        inputs.forEach(cell => {
            maxRow = Math.max(maxRow, cell.row || 0);
            maxCol = Math.max(maxCol, cell.col || 0);
        });
        const rows = maxRow + 1;
        const cols = maxCol + 1;

        // Build a 2D grid from inputs
        const grid = [];
        for (let r = 0; r < rows; r++) {
            grid[r] = new Array(cols).fill(null);
        }

        // Track which columns have block cells
        const blockColumns = new Set();
        inputs.forEach(cell => {
            const normalized = normalizeCell(cell);
            const r = normalized.row;
            const c = normalized.col;
            if (r < rows && c < cols) {
                grid[r][c] = normalized;
                if (normalized.isBlock) {
                    blockColumns.add(c);
                }
            }
        });

        // Determine which rows have outputs (for wire connection)
        const outputRowSet = new Set(outputs.map(c => c.row || 0));

        // Render input rows
        let rowsHtml = '';
        for (let r = 0; r < rows; r++) {
            const rowClass = r === 0 ? 'ladder-row ladder-row-main' : 'ladder-row ladder-row-branch';
            let rowCellsHtml = '';

            for (let c = 0; c < cols; c++) {
                const cell = grid[r][c];
                const isBlockCol = blockColumns.has(c);

                if (cell) {
                    rowCellsHtml += createCellHtml(cell, isBlockCol);
                } else {
                    rowCellsHtml += createEmptyHtml(r, c, isBlockCol);
                }
            }

            rowsHtml += `<div class="${rowClass}" data-row="${r}">${rowCellsHtml}</div>`;
        }

        // Calculate output row metrics first (needed for both wire connector and outputs)
        const outputRowNums = outputs.map(c => c.row || 0);
        const maxOutputRow = outputs.length > 0 ? Math.max(...outputRowNums) : 0;
        const minOutputRow = outputs.length > 0 ? Math.min(...outputRowNums) : 0;
        const hasMultipleOutputs = outputs.length > 1;
        // Use the greater of input rows or output rows for alignment
        const totalRows = Math.max(rows, maxOutputRow + 1);

        // Build output grid (similar to input grid)
        // Outputs may include branch connectors at col 0 and coils at col 1
        let maxOutputCol = 0;
        outputs.forEach(cell => {
            maxOutputCol = Math.max(maxOutputCol, cell.col || 0);
        });
        const outputCols = maxOutputCol + 1;

        // Build output grid
        const outputGrid = [];
        for (let r = 0; r < totalRows; r++) {
            outputGrid[r] = new Array(outputCols).fill(null);
        }
        outputs.forEach(cell => {
            const normalized = normalizeCell(cell);
            const r = normalized.row;
            const c = normalized.col;
            if (r < totalRows && c < outputCols) {
                outputGrid[r][c] = normalized;
            }
        });

        // Render outputs as a grid (branch connectors + coils)
        let outputsHtml = '';
        for (let r = 0; r < totalRows; r++) {
            let outputRowHtml = '';
            for (let c = 0; c < outputCols; c++) {
                const cell = outputGrid[r][c];
                if (cell) {
                    outputRowHtml += createCellHtml(cell, false);
                } else {
                    // Empty placeholder - use spacer for branch connector column
                    if (c === 0 && outputCols > 1) {
                        outputRowHtml += `<div class="ladder-output-spacer ladder-vline-cell" data-row="${r}" data-col="${c}"></div>`;
                    } else {
                        outputRowHtml += `<div class="ladder-output-spacer" data-row="${r}" data-col="${c}"></div>`;
                    }
                }
            }
            outputsHtml += `<div class="ladder-output-row" data-row="${r}">${outputRowHtml}</div>`;
        }

        // Create simple wire connector (horizontal line from inputs to outputs)
        let wireConnectorHtml = '';
        for (let r = 0; r < totalRows; r++) {
            const hasOutput = outputRowNums.includes(r);
            // Only draw horizontal wire for rows that have outputs or row 0
            const wireType = (hasOutput || r === 0) ? 'horizontal' : 'empty';
            wireConnectorHtml += `<div class="ladder-wire-row" data-row="${r}" data-wire-type="${wireType}"></div>`;
        }

        return `
            <div class="ladder-rung" id="rung-${rungNum}" data-rungnum="${rungNum}" data-rows="${rows}" data-cols="${cols}">
                <div class="rung-header">
                    <span class="rung-number">Network ${rungNum}</span>
                    ${comment ? `<span class="rung-comment">${escapeHtml(comment)}</span>` : ''}
                </div>
                <div class="ladder-grid" style="--cols: ${cols}; --rows: ${rows};">
                    <div class="power-rail left"></div>
                    <div class="ladder-content">
                        ${rowsHtml}
                    </div>
                    <div class="ladder-wire-connector">
                        ${wireConnectorHtml}
                    </div>
                    <div class="ladder-outputs">
                        ${outputsHtml}
                    </div>
                    <div class="power-rail right"></div>
                </div>
            </div>
        `;
    }

    /**
     * Create HTML for a single rung (legacy format with branch computation)
     * @param {Object} rung - Rung data
     * @returns {string} - HTML string
     */
    function createRungHtmlLegacy(rung) {
        const rungNum = rung.rungnum;
        const cells = rung.cells || [];
        const comment = rung.comment || '';
        const rows = rung.rows || 1;
        const cols = rung.cols || 1;
        const branches = rung.branches || [];
        const outputBranches = rung.outputBranches || [];

        // Build a 2D grid of cells
        const grid = [];
        for (let r = 0; r < rows; r++) {
            grid[r] = new Array(cols).fill(null);
        }

        // Place cells in grid and track which columns have blocks
        const blockColumns = new Set();
        cells.forEach(cell => {
            const normalized = normalizeCell(cell);
            const row = normalized.row;
            const col = normalized.col;
            if (row < rows && col < cols) {
                grid[row][col] = normalized;
                if (normalized.isBlock) {
                    blockColumns.add(col);
                }
            }
        });

        // Build branch merge info
        const mergeColInfo = {};
        const rowMergeCol = {};
        branches.forEach(branch => {
            const mergeCol = branch.mergeCol;
            const branchRow = branch.row;
            if (mergeCol !== undefined) {
                if (!mergeColInfo[mergeCol]) {
                    mergeColInfo[mergeCol] = { rows: [] };
                }
                mergeColInfo[mergeCol].rows.push(branchRow);
                rowMergeCol[branchRow] = mergeCol;
            }
        });

        // Render rows
        let rowsHtml = '';
        for (let r = 0; r < rows; r++) {
            const rowClass = r === 0 ? 'ladder-row ladder-row-main' : 'ladder-row ladder-row-branch';
            let rowCellsHtml = '';
            const thisBranchMergeCol = rowMergeCol[r] || cols;

            for (let c = 0; c < cols; c++) {
                const cell = grid[r][c];
                const mergeInfo = mergeColInfo[c];
                const isAtMergeCol = mergeInfo && mergeInfo.rows.length > 0;
                const branchRowsAtThisCol = isAtMergeCol ? mergeInfo.rows : [];
                const maxBranchRow = branchRowsAtThisCol.length > 0 ? Math.max(...branchRowsAtThisCol) : 0;
                const isBlockCol = blockColumns.has(c);
                const isBeforeMerge = c < thisBranchMergeCol;
                const isAtMerge = c === thisBranchMergeCol;

                if (cell) {
                    rowCellsHtml += createCellHtml(cell, isBlockCol);
                } else if (r === 0) {
                    if (isAtMergeCol && maxBranchRow > 0) {
                        rowCellsHtml += createBranchCellHtml(r, c, 'branchDown', isBlockCol);
                    } else {
                        rowCellsHtml += createSpacerHtml(r, c, isBlockCol);
                    }
                } else {
                    const needsVerticalFromAbove = isAtMergeCol && r <= maxBranchRow;
                    const needsVerticalToBelow = isAtMergeCol && r < maxBranchRow;
                    const isBranchMergeRow = branchRowsAtThisCol.includes(r);

                    if (isBranchMergeRow && isAtMerge) {
                        if (needsVerticalToBelow) {
                            rowCellsHtml += createBranchCellHtml(r, c, 'branchMerge', isBlockCol);
                        } else {
                            rowCellsHtml += createBranchCellHtml(r, c, 'branchUp', isBlockCol);
                        }
                    } else if (needsVerticalFromAbove && needsVerticalToBelow && !isBeforeMerge) {
                        rowCellsHtml += createBranchCellHtml(r, c, 'vline', isBlockCol);
                    } else if (isBeforeMerge) {
                        rowCellsHtml += createSpacerHtml(r, c, isBlockCol);
                    } else {
                        rowCellsHtml += createEmptyHtml(r, c, isBlockCol);
                    }
                }
            }

            rowsHtml += `<div class="${rowClass}" data-row="${r}">${rowCellsHtml}</div>`;
        }

        return `
            <div class="ladder-rung" id="rung-${rungNum}" data-rungnum="${rungNum}" data-rows="${rows}" data-cols="${cols}">
                <div class="rung-header">
                    <span class="rung-number">Network ${rungNum}</span>
                    ${comment ? `<span class="rung-comment">${escapeHtml(comment)}</span>` : ''}
                </div>
                <div class="ladder-grid" style="--cols: ${cols}; --rows: ${rows};">
                    <div class="power-rail left"></div>
                    <div class="ladder-content">
                        ${rowsHtml}
                    </div>
                    <div class="power-rail right"></div>
                </div>
            </div>
        `;
    }

    /**
     * Create a branch/vline cell HTML (for legacy format)
     */
    function createBranchCellHtml(row, col, symbolType, isBlockWidth = false) {
        const svgHtml = LadSymbols.getSymbol(symbolType, 'MB_ladderoff');
        const widthClass = isBlockWidth ? 'ladder-vline-cell ladder-vline-block' : 'ladder-vline-cell';
        return `<div class="${widthClass}" data-row="${row}" data-col="${col}" data-symbol="${symbolType}">
            ${svgHtml}
        </div>`;
    }

    /**
     * Create a horizontal spacer cell (for legacy format)
     */
    function createSpacerHtml(row, col, isBlockWidth = false) {
        const widthClass = isBlockWidth ? 'ladder-spacer ladder-spacer-block' : 'ladder-spacer';
        return `<div class="${widthClass}" data-row="${row}" data-col="${col}">
            ${LadSymbols.getSymbol('hline', 'MB_ladderoff')}
        </div>`;
    }

    /**
     * Create HTML for a rung (auto-detects format)
     * @param {Object} rung - Rung data
     * @returns {string} - HTML string
     */
    function createRungHtml(rung) {
        if (isMatrixdataFormat(rung)) {
            return createRungHtmlMatrixdata(rung);
        } else {
            return createRungHtmlLegacy(rung);
        }
    }

    /**
     * Create the complete rung list from program data
     * @param {Object} programData - Program data from API
     * @returns {string} - HTML string for all rungs
     */
    function createRungList(programData) {
        setProgram(programData);

        if (!programData || !programData.subrdata) {
            return `
                <div class="no-program">
                    <h3>No Program Loaded</h3>
                    <p>Start the web server with a program:</p>
                    <code>(mblogic-cl-web:quick-start "test/plcprog.txt" :port 8080)</code>
                </div>
            `;
        }

        if (programData.error) {
            return `<div class="error-message">${escapeHtml(programData.error)}</div>`;
        }

        const rungs = programData.subrdata;
        if (!rungs || rungs.length === 0) {
            return '<div class="no-program"><p>No rungs in this subroutine</p></div>';
        }

        let html = '';
        rungs.forEach(rung => {
            html += createRungHtml(rung);
        });

        return html;
    }

    /**
     * Render the program to the display container
     * @param {string} containerId - ID of container element
     * @param {Object} programData - Program data from API
     */
    function renderToContainer(containerId, programData) {
        const container = document.getElementById(containerId);
        if (container) {
            container.innerHTML = createRungList(programData);
        }
    }

    /**
     * Update SVG element classes within a container
     * @param {Element} container - Container element with SVG
     * @param {boolean} isOn - Whether the element is energized
     */
    function updateSvgClasses(container, isOn) {
        const stateClass = isOn ? 'MB_ladderon' : 'MB_ladderoff';
        const removeClass = isOn ? 'MB_ladderoff' : 'MB_ladderon';

        const svg = container.querySelector('svg');
        if (svg) {
            svg.classList.remove(removeClass);
            svg.classList.add(stateClass);

            svg.querySelectorAll('line, circle, rect, text').forEach(el => {
                el.classList.remove(removeClass);
                el.classList.add(stateClass);
            });
        }
    }

    /**
     * Update cell states based on data values
     * @param {Object} dataValues - Object mapping addresses to values
     */
    function updateCellStates(dataValues) {
        if (!dataValues || typeof dataValues !== 'object') {
            return;
        }

        document.querySelectorAll('.ladder-cell').forEach(cell => {
            const addressesStr = cell.getAttribute('data-addresses');
            if (!addressesStr) return;

            const addresses = addressesStr.split(',').filter(a => a.trim());
            if (addresses.length === 0) return;

            let isOn = false;
            let displayValue = null;

            addresses.forEach(addr => {
                const value = dataValues[addr];
                if (value !== undefined) {
                    if (typeof value === 'boolean') {
                        if (value) isOn = true;
                    } else if (typeof value === 'number') {
                        displayValue = value;
                        if (value !== 0) isOn = true;
                    } else if (value === 'true' || value === true) {
                        isOn = true;
                    } else if (value !== null && value !== undefined && value !== 'false' && value !== false) {
                        displayValue = value;
                    }
                }
            });

            cell.classList.remove('MB_ladderoff', 'MB_ladderon');
            cell.classList.add(isOn ? 'MB_ladderon' : 'MB_ladderoff');

            updateSvgClasses(cell, isOn);

            const valueEl = cell.querySelector('.cell-value');
            if (valueEl) {
                if (displayValue !== null) {
                    valueEl.textContent = formatValue(displayValue);
                    valueEl.style.display = 'block';
                } else {
                    valueEl.style.display = 'none';
                }
            }
        });
    }

    /**
     * Format a value for display
     * @param {*} value - Value to format
     * @returns {string} - Formatted string
     */
    function formatValue(value) {
        if (typeof value === 'number') {
            if (Number.isInteger(value)) {
                return value.toString();
            }
            return value.toFixed(2);
        }
        return String(value);
    }

    /**
     * Escape HTML special characters
     * @param {string} str - Input string
     * @returns {string} - Escaped string
     */
    function escapeHtml(str) {
        if (!str) return '';
        const div = document.createElement('div');
        div.textContent = str;
        return div.innerHTML;
    }

    // Public API
    return {
        setProgram,
        getMonitorAddresses,
        createRungList,
        renderToContainer,
        updateCellStates,
        isBlockSymbol
    };
})();
