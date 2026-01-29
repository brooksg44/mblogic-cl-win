/**
 * ladsubrdisplib.js - Ladder Subroutine Display Library
 * Handles rendering ladder diagram rungs from program data
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
            // Collect addresses from the program-level addresses array
            currentAddresses = programData.addresses.slice();
        }

        // Also collect addresses from all cells in case program.addresses is incomplete
        if (programData && programData.subrdata) {
            programData.subrdata.forEach(rung => {
                if (rung.addrs) {
                    rung.addrs.forEach(addr => {
                        if (addr && !currentAddresses.includes(addr)) {
                            currentAddresses.push(addr);
                        }
                    });
                }
                if (rung.cells) {
                    rung.cells.forEach(cell => {
                        if (cell.addrs) {
                            cell.addrs.forEach(addr => {
                                if (addr && !currentAddresses.includes(addr)) {
                                    currentAddresses.push(addr);
                                }
                            });
                        }
                        if (cell.addr && !currentAddresses.includes(cell.addr)) {
                            currentAddresses.push(cell.addr);
                        }
                    });
                }
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
     * Create HTML for a single cell
     * @param {Object} cell - Cell data
     * @param {boolean} forceBlockWidth - Force block width even for non-block cells
     * @returns {string} - HTML string
     */
    function createCellHtml(cell, forceBlockWidth = false) {
        const symbol = cell.symbol || 'il';
        const address = cell.addr || '';
        const addresses = cell.addrs || [];
        const opcode = cell.opcode || '';
        const params = cell.params || [];
        const cellType = cell.type || 'unknown';
        const isBlock = isBlockSymbol(symbol) || cellType === 'block';
        const row = cell.row || 0;
        const col = cell.col || 0;

        // Create unique ID for monitoring
        const cellId = `cell-${row}-${col}`;

        // Determine cell class and dimensions
        // Use block width if cell is a block OR if column contains a block (forceBlockWidth)
        const useBlockWidth = isBlock || forceBlockWidth;
        const cellClass = useBlockWidth ? 'ladder-cell ladder-block-cell' : 'ladder-cell';

        // Get SVG symbol
        const svgHtml = LadSymbols.getSymbol(symbol, 'MB_ladderoff');

        // Create address display
        let addressDisplay = '';
        if (address) {
            addressDisplay = `<span class="cell-address">${escapeHtml(address)}</span>`;
        } else if (addresses.length > 0) {
            addressDisplay = `<span class="cell-address">${escapeHtml(addresses[0])}</span>`;
        }

        // For block types, show parameters below the symbol
        let paramsDisplay = '';
        if (isBlock && params.length > 0) {
            const displayParams = params.slice(0, 4).join(' ');
            paramsDisplay = `<span class="block-params">${escapeHtml(displayParams)}</span>`;
        }

        // Data attributes for monitoring
        const dataAttrs = addresses.length > 0
            ? `data-addresses="${escapeHtml(addresses.join(','))}"`
            : '';

        return `
            <div id="${cellId}" class="${cellClass} MB_ladderoff"
                 ${dataAttrs}
                 data-opcode="${escapeHtml(opcode)}"
                 data-symbol="${escapeHtml(symbol)}"
                 data-row="${row}"
                 data-col="${col}">
                <div class="cell-symbol">
                    ${svgHtml}
                </div>
                ${paramsDisplay}
                ${addressDisplay}
                <span class="cell-value"></span>
            </div>
        `;
    }

    /**
     * Create an empty spacer cell for alignment
     * @param {number} row - Row index
     * @param {number} col - Column index
     * @param {boolean} isBlockWidth - If true, use block cell width
     * @returns {string} - HTML string
     */
    function createSpacerHtml(row, col, isBlockWidth = false) {
        const widthClass = isBlockWidth ? 'ladder-spacer ladder-spacer-block' : 'ladder-spacer';
        return `<div class="${widthClass}" data-row="${row}" data-col="${col}">
            ${LadSymbols.getSymbol('hline', 'MB_ladderoff')}
        </div>`;
    }

    /**
     * Create a vertical line cell for branch connections
     * @param {number} row - Row index
     * @param {number} col - Column index
     * @param {string} symbolType - Symbol type: 'vline', 'branchDown', 'branchUp', 'branchMerge', 'branchStart'
     * @param {boolean} isBlockWidth - If true, use block cell width
     * @returns {string} - HTML string
     */
    function createVlineCellHtml(row, col, symbolType, isBlockWidth = false) {
        const svgHtml = LadSymbols.getSymbol(symbolType, 'MB_ladderoff');
        const widthClass = isBlockWidth ? 'ladder-vline-cell ladder-vline-block' : 'ladder-vline-cell';
        return `<div class="${widthClass}" data-row="${row}" data-col="${col}" data-symbol="${symbolType}">
            ${svgHtml}
        </div>`;
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
     * Create HTML for a single rung with proper branch handling
     * @param {Object} rung - Rung data
     * @returns {string} - HTML string
     */
    function createRungHtml(rung) {
        const rungNum = rung.rungnum;
        const cells = rung.cells || [];
        const comment = rung.comment || '';
        const rows = rung.rows || 1;
        const cols = rung.cols || 1;
        const branches = rung.branches || [];

        // Build a 2D grid of cells
        const grid = [];
        for (let r = 0; r < rows; r++) {
            grid[r] = new Array(cols).fill(null);
        }

        // Place cells in grid and track which columns have blocks
        const blockColumns = new Set();
        cells.forEach(cell => {
            const row = cell.row || 0;
            const col = cell.col || 0;
            if (row < rows && col < cols) {
                grid[row][col] = cell;
                // Track columns that contain block-type cells
                if (isBlockSymbol(cell.symbol) || cell.type === 'block') {
                    blockColumns.add(col);
                }
            }
        });

        // Build a map of merge columns for quick lookup
        // mergeColInfo[col] = { rows: [list of branch rows that merge here] }
        const mergeColInfo = {};
        // Also track which row merges at which column
        const rowMergeCol = {};
        branches.forEach(branch => {
            const mergeCol = branch.mergeCol;  // JSON uses camelCase
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

            // For branch rows, determine where this row merges
            const thisBranchMergeCol = rowMergeCol[r] || cols;

            for (let c = 0; c < cols; c++) {
                const cell = grid[r][c];
                const mergeInfo = mergeColInfo[c];
                const isAtMergeCol = mergeInfo && mergeInfo.rows.length > 0;
                const branchRowsAtThisCol = isAtMergeCol ? mergeInfo.rows : [];
                const maxBranchRow = branchRowsAtThisCol.length > 0 ? Math.max(...branchRowsAtThisCol) : 0;
                const isBlockCol = blockColumns.has(c);

                if (cell) {
                    // Pass forceBlockWidth if this column contains a block anywhere
                    rowCellsHtml += createCellHtml(cell, isBlockCol);
                } else if (r === 0) {
                    // Main row: check if this is a merge point (need vertical line down)
                    if (isAtMergeCol && maxBranchRow > 0) {
                        // This is where branches connect - show branchDown symbol
                        rowCellsHtml += createVlineCellHtml(r, c, 'branchDown', isBlockCol);
                    } else {
                        // Regular horizontal line
                        rowCellsHtml += createSpacerHtml(r, c, isBlockCol);
                    }
                } else {
                    // Branch row
                    const needsVerticalFromAbove = isAtMergeCol && r <= maxBranchRow;
                    const needsVerticalToBelow = isAtMergeCol && r < maxBranchRow;
                    const isBranchMergeRow = branchRowsAtThisCol.includes(r);

                    // On branch rows, we only render up to and including the merge column
                    const isBeforeMerge = c < thisBranchMergeCol;
                    const isAtMerge = c === thisBranchMergeCol;

                    if (isBranchMergeRow && isAtMerge) {
                        // This is the merge point for this branch row
                        if (needsVerticalToBelow) {
                            rowCellsHtml += createVlineCellHtml(r, c, 'branchMerge', isBlockCol);
                        } else {
                            rowCellsHtml += createVlineCellHtml(r, c, 'branchUp', isBlockCol);
                        }
                    } else if (needsVerticalFromAbove && needsVerticalToBelow && !isBeforeMerge) {
                        // Vertical pass-through for branches below this one
                        rowCellsHtml += createVlineCellHtml(r, c, 'vline', isBlockCol);
                    } else if (isBeforeMerge) {
                        // Before merge point: show horizontal line to connect contact to merge
                        rowCellsHtml += createSpacerHtml(r, c, isBlockCol);
                    } else {
                        // After merge point on branch row: empty (branch has ended)
                        rowCellsHtml += createEmptyHtml(r, c, isBlockCol);
                    }
                }
            }

            rowsHtml += `<div class="${rowClass}" data-row="${r}">${rowCellsHtml}</div>`;
        }

        // Build complete rung HTML (no CSS branch connectors needed anymore)
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

            // Update all child elements (lines, circles, rects, text)
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

        // Find all cells with monitored addresses
        document.querySelectorAll('.ladder-cell').forEach(cell => {
            const addressesStr = cell.getAttribute('data-addresses');
            if (!addressesStr) return;

            const addresses = addressesStr.split(',').filter(a => a.trim());
            if (addresses.length === 0) return;

            // Check if any address is "on"
            let isOn = false;
            let displayValue = null;

            addresses.forEach(addr => {
                // Handle both direct property access and potential JSON key issues
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

            // Update CSS class on the cell
            cell.classList.remove('MB_ladderoff', 'MB_ladderon');
            cell.classList.add(isOn ? 'MB_ladderon' : 'MB_ladderoff');

            // Update SVG elements inside
            updateSvgClasses(cell, isOn);

            // Update value display if applicable
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

        // Also update vline cells for branch visualization
        // For now, vline cells stay in their default state since they don't have addresses
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
