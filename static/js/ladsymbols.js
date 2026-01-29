/**
 * ladsymbols.js - Ladder Diagram SVG Symbol Definitions
 * Provides SVG template strings for all ladder diagram symbols
 */

const LadSymbols = (function() {
    'use strict';

    // Symbol dimensions
    const WIDTH = 60;
    const HEIGHT = 40;
    const CENTER_Y = HEIGHT / 2;

    // Block dimensions (wider for instructions with parameters)
    const BLOCK_WIDTH = 120;
    const BLOCK_HEIGHT = 60;

    /**
     * Create an SVG wrapper with the given content
     * @param {string} content - SVG content
     * @param {string} stateClass - State class (MB_ladderoff or MB_ladderon)
     * @param {number} width - SVG width (default: WIDTH)
     * @param {number} height - SVG height (default: HEIGHT)
     * @returns {string} - Complete SVG element string
     */
    function wrapSvg(content, stateClass = 'MB_ladderoff', width = WIDTH, height = HEIGHT) {
        // Use preserveAspectRatio="none" so lines stretch to fill cell edges
        return `<svg viewBox="0 0 ${width} ${height}" preserveAspectRatio="none" class="ladder-symbol ${stateClass}">
            ${content}
        </svg>`;
    }

    // Vertical line cell height (matches CSS .ladder-vline-cell height)
    const VLINE_HEIGHT = 80;

    /**
     * Create a block instruction SVG
     * @param {string} label - Block label (opcode)
     * @param {string} stateClass - State class
     * @returns {string} - SVG element string
     */
    function createBlockSvg(label, stateClass = 'MB_ladderoff') {
        const centerY = BLOCK_HEIGHT / 2;
        return wrapSvg(`
            <line x1="0" y1="${centerY}" x2="10" y2="${centerY}" class="${stateClass}"/>
            <rect x="10" y="5" width="100" height="50" class="${stateClass}" fill="none" rx="3" ry="3"/>
            <text x="60" y="35" text-anchor="middle" font-size="11" font-weight="bold" class="${stateClass}">${label}</text>
            <line x1="110" y1="${centerY}" x2="${BLOCK_WIDTH}" y2="${centerY}" class="${stateClass}"/>
        `, stateClass, BLOCK_WIDTH, BLOCK_HEIGHT);
    }

    // Symbol definitions
    const symbols = {
        /**
         * Normally Open Contact (NO)
         * --| |--
         */
        noc: (stateClass) => wrapSvg(`
            <line x1="0" y1="${CENTER_Y}" x2="15" y2="${CENTER_Y}" class="${stateClass}"/>
            <line x1="15" y1="8" x2="15" y2="32" class="${stateClass}"/>
            <line x1="45" y1="8" x2="45" y2="32" class="${stateClass}"/>
            <line x1="45" y1="${CENTER_Y}" x2="${WIDTH}" y2="${CENTER_Y}" class="${stateClass}"/>
        `, stateClass),

        /**
         * Normally Closed Contact (NC)
         * --|/|--
         */
        ncc: (stateClass) => wrapSvg(`
            <line x1="0" y1="${CENTER_Y}" x2="15" y2="${CENTER_Y}" class="${stateClass}"/>
            <line x1="15" y1="8" x2="15" y2="32" class="${stateClass}"/>
            <line x1="45" y1="8" x2="45" y2="32" class="${stateClass}"/>
            <line x1="18" y1="30" x2="42" y2="10" class="${stateClass}"/>
            <line x1="45" y1="${CENTER_Y}" x2="${WIDTH}" y2="${CENTER_Y}" class="${stateClass}"/>
        `, stateClass),

        /**
         * Positive Differential Contact (Rising Edge)
         * --|P|--
         */
        nocpd: (stateClass) => wrapSvg(`
            <line x1="0" y1="${CENTER_Y}" x2="15" y2="${CENTER_Y}" class="${stateClass}"/>
            <line x1="15" y1="8" x2="15" y2="32" class="${stateClass}"/>
            <line x1="45" y1="8" x2="45" y2="32" class="${stateClass}"/>
            <text x="30" y="24" text-anchor="middle" font-size="12" font-weight="bold" class="${stateClass}">P</text>
            <line x1="45" y1="${CENTER_Y}" x2="${WIDTH}" y2="${CENTER_Y}" class="${stateClass}"/>
        `, stateClass),

        /**
         * Negative Differential Contact (Falling Edge)
         * --|N|--
         */
        nocnd: (stateClass) => wrapSvg(`
            <line x1="0" y1="${CENTER_Y}" x2="15" y2="${CENTER_Y}" class="${stateClass}"/>
            <line x1="15" y1="8" x2="15" y2="32" class="${stateClass}"/>
            <line x1="45" y1="8" x2="45" y2="32" class="${stateClass}"/>
            <text x="30" y="24" text-anchor="middle" font-size="12" font-weight="bold" class="${stateClass}">N</text>
            <line x1="45" y1="${CENTER_Y}" x2="${WIDTH}" y2="${CENTER_Y}" class="${stateClass}"/>
        `, stateClass),

        /**
         * Output Coil
         * --( )--
         */
        out: (stateClass) => wrapSvg(`
            <line x1="0" y1="${CENTER_Y}" x2="15" y2="${CENTER_Y}" class="${stateClass}"/>
            <circle cx="30" cy="${CENTER_Y}" r="12" class="${stateClass}" fill="none"/>
            <line x1="45" y1="${CENTER_Y}" x2="${WIDTH}" y2="${CENTER_Y}" class="${stateClass}"/>
        `, stateClass),

        /**
         * Set (Latch) Coil
         * --(S)--
         */
        set: (stateClass) => wrapSvg(`
            <line x1="0" y1="${CENTER_Y}" x2="15" y2="${CENTER_Y}" class="${stateClass}"/>
            <circle cx="30" cy="${CENTER_Y}" r="12" class="${stateClass}" fill="none"/>
            <text x="30" y="24" text-anchor="middle" font-size="10" font-weight="bold" class="${stateClass}">S</text>
            <line x1="45" y1="${CENTER_Y}" x2="${WIDTH}" y2="${CENTER_Y}" class="${stateClass}"/>
        `, stateClass),

        /**
         * Reset (Unlatch) Coil
         * --(R)--
         */
        rst: (stateClass) => wrapSvg(`
            <line x1="0" y1="${CENTER_Y}" x2="15" y2="${CENTER_Y}" class="${stateClass}"/>
            <circle cx="30" cy="${CENTER_Y}" r="12" class="${stateClass}" fill="none"/>
            <text x="30" y="24" text-anchor="middle" font-size="10" font-weight="bold" class="${stateClass}">R</text>
            <line x1="45" y1="${CENTER_Y}" x2="${WIDTH}" y2="${CENTER_Y}" class="${stateClass}"/>
        `, stateClass),

        /**
         * Pulse/Differentiate Coil
         * --(P)--
         */
        pd: (stateClass) => wrapSvg(`
            <line x1="0" y1="${CENTER_Y}" x2="15" y2="${CENTER_Y}" class="${stateClass}"/>
            <circle cx="30" cy="${CENTER_Y}" r="12" class="${stateClass}" fill="none"/>
            <text x="30" y="24" text-anchor="middle" font-size="10" font-weight="bold" class="${stateClass}">P</text>
            <line x1="45" y1="${CENTER_Y}" x2="${WIDTH}" y2="${CENTER_Y}" class="${stateClass}"/>
        `, stateClass),

        /**
         * Comparison Block
         * --[CMP]--
         */
        compare: (stateClass) => createBlockSvg('CMP', stateClass),

        /**
         * Timer On-Delay
         */
        tmr: (stateClass) => createBlockSvg('TMR', stateClass),

        /**
         * Timer Accumulating
         */
        tmra: (stateClass) => createBlockSvg('TMRA', stateClass),

        /**
         * Timer Off-Delay
         */
        tmroff: (stateClass) => createBlockSvg('TMROFF', stateClass),

        /**
         * Counter Up
         */
        cntu: (stateClass) => createBlockSvg('CNTU', stateClass),

        /**
         * Counter Down
         */
        cntd: (stateClass) => createBlockSvg('CNTD', stateClass),

        /**
         * Up/Down Counter
         */
        udc: (stateClass) => createBlockSvg('UDC', stateClass),

        /**
         * Copy instruction
         */
        copy: (stateClass) => createBlockSvg('COPY', stateClass),

        /**
         * Block Copy
         */
        cpyblk: (stateClass) => createBlockSvg('CPYBLK', stateClass),

        /**
         * Fill instruction
         */
        fill: (stateClass) => createBlockSvg('FILL', stateClass),

        /**
         * Pack bits
         */
        pack: (stateClass) => createBlockSvg('PACK', stateClass),

        /**
         * Unpack bits
         */
        unpack: (stateClass) => createBlockSvg('UNPACK', stateClass),

        /**
         * Shift Register
         */
        shfrg: (stateClass) => createBlockSvg('SHFRG', stateClass),

        /**
         * Find Equal
         */
        findeq: (stateClass) => createBlockSvg('FINDEQ', stateClass),

        /**
         * Math Decimal
         */
        mathdec: (stateClass) => createBlockSvg('MATH', stateClass),

        /**
         * Math Hex
         */
        mathhex: (stateClass) => createBlockSvg('MATHX', stateClass),

        /**
         * Sum
         */
        sum: (stateClass) => createBlockSvg('SUM', stateClass),

        /**
         * Subroutine Call
         */
        call: (stateClass) => createBlockSvg('CALL', stateClass),

        /**
         * Return
         */
        rt: (stateClass) => createBlockSvg('RT', stateClass),

        /**
         * End
         */
        end: (stateClass) => createBlockSvg('END', stateClass),

        /**
         * For Loop Start
         */
        for: (stateClass) => createBlockSvg('FOR', stateClass),

        /**
         * For Loop End (Next)
         */
        next: (stateClass) => createBlockSvg('NEXT', stateClass),

        /**
         * Horizontal line (for spacing/continuation)
         */
        hline: (stateClass) => wrapSvg(`
            <line x1="0" y1="${CENTER_Y}" x2="${WIDTH}" y2="${CENTER_Y}" class="${stateClass}"/>
        `, stateClass),

        /**
         * Empty cell (just horizontal line)
         */
        empty: (stateClass) => wrapSvg(`
            <line x1="0" y1="${CENTER_Y}" x2="${WIDTH}" y2="${CENTER_Y}" class="${stateClass}"/>
        `, stateClass),

        /**
         * IL instruction fallback display
         */
        il: (stateClass) => wrapSvg(`
            <line x1="0" y1="${CENTER_Y}" x2="5" y2="${CENTER_Y}" class="${stateClass}"/>
            <rect x="5" y="5" width="50" height="30" class="${stateClass}" fill="none" stroke-dasharray="3,2"/>
            <text x="30" y="24" text-anchor="middle" font-size="8" class="${stateClass}">IL</text>
            <line x1="55" y1="${CENTER_Y}" x2="${WIDTH}" y2="${CENTER_Y}" class="${stateClass}"/>
        `, stateClass),

        /**
         * Vertical line (for branch connections spanning full cell height)
         * Uses VLINE_HEIGHT for proper vertical connection between rows
         */
        vline: (stateClass) => wrapSvg(`
            <line x1="30" y1="0" x2="30" y2="${VLINE_HEIGHT}" class="${stateClass}"/>
        `, stateClass, WIDTH, VLINE_HEIGHT),

        /**
         * Branch down: horizontal line continuing, then vertical down to next row
         * Used at the top of a branch (main row) - merge point
         * ----+----
         *     |
         */
        branchDown: (stateClass) => {
            const centerY = VLINE_HEIGHT / 2;
            return wrapSvg(`
                <line x1="0" y1="${centerY}" x2="${WIDTH}" y2="${centerY}" class="${stateClass}"/>
                <line x1="30" y1="${centerY}" x2="30" y2="${VLINE_HEIGHT}" class="${stateClass}"/>
            `, stateClass, WIDTH, VLINE_HEIGHT);
        },

        /**
         * Branch up: vertical line coming from above, then horizontal going LEFT only
         * Used at the bottom of a branch (last branch row at merge point)
         *     |
         * ----+
         */
        branchUp: (stateClass) => {
            const centerY = VLINE_HEIGHT / 2;
            return wrapSvg(`
                <line x1="30" y1="0" x2="30" y2="${centerY}" class="${stateClass}"/>
                <line x1="0" y1="${centerY}" x2="30" y2="${centerY}" class="${stateClass}"/>
            `, stateClass, WIDTH, VLINE_HEIGHT);
        },

        /**
         * Branch merge: vertical line through cell with horizontal going LEFT only
         * Used for middle rows in multi-branch setups
         *     |
         * ----+
         *     |
         */
        branchMerge: (stateClass) => {
            const centerY = VLINE_HEIGHT / 2;
            return wrapSvg(`
                <line x1="30" y1="0" x2="30" y2="${VLINE_HEIGHT}" class="${stateClass}"/>
                <line x1="0" y1="${centerY}" x2="30" y2="${centerY}" class="${stateClass}"/>
            `, stateClass, WIDTH, VLINE_HEIGHT);
        },

        /**
         * Branch start: vertical from above connecting to horizontal going right
         * Used at column 0 of branch rows to show connection from power rail
         *     |
         * +-------
         */
        branchStart: (stateClass) => {
            const centerY = VLINE_HEIGHT / 2;
            return wrapSvg(`
                <line x1="30" y1="0" x2="30" y2="${centerY}" class="${stateClass}"/>
                <line x1="30" y1="${centerY}" x2="${WIDTH}" y2="${centerY}" class="${stateClass}"/>
            `, stateClass, WIDTH, VLINE_HEIGHT);
        },

        /**
         * Output branch middle: vertical through + horizontal to right
         * Used for middle rows in parallel output coils
         *     |
         *     +-------
         *     |
         */
        outputBranchMid: (stateClass) => {
            const centerY = VLINE_HEIGHT / 2;
            return wrapSvg(`
                <line x1="30" y1="0" x2="30" y2="${VLINE_HEIGHT}" class="${stateClass}"/>
                <line x1="30" y1="${centerY}" x2="${WIDTH}" y2="${centerY}" class="${stateClass}"/>
            `, stateClass, WIDTH, VLINE_HEIGHT);
        }
    };

    /**
     * Get SVG for a symbol
     * @param {string} symbolName - Symbol name
     * @param {string} stateClass - State class (MB_ladderoff or MB_ladderon)
     * @returns {string} - SVG element string
     */
    function getSymbol(symbolName, stateClass = 'MB_ladderoff') {
        const symbolFn = symbols[symbolName] || symbols.il;
        return symbolFn(stateClass);
    }

    /**
     * Get list of all symbol names
     * @returns {string[]} - Array of symbol names
     */
    function getSymbolNames() {
        return Object.keys(symbols);
    }

    /**
     * Check if a symbol is a block type (wider)
     * @param {string} symbolName - Symbol name
     * @returns {boolean}
     */
    function isBlockSymbol(symbolName) {
        const blockSymbols = [
            'compare', 'tmr', 'tmra', 'tmroff',
            'cntu', 'cntd', 'udc',
            'copy', 'cpyblk', 'fill',
            'pack', 'unpack', 'shfrg',
            'findeq', 'mathdec', 'mathhex', 'sum',
            'call', 'rt', 'end', 'for', 'next'
        ];
        return blockSymbols.includes(symbolName);
    }

    // Public API
    return {
        getSymbol,
        getSymbolNames,
        isBlockSymbol,
        WIDTH,
        HEIGHT,
        BLOCK_WIDTH,
        BLOCK_HEIGHT
    };
})();
