/**
 * ladmonitor.js - Live Ladder Diagram Monitoring
 * Handles periodic data polling and display updates
 */

const LadMonitor = (function() {
    'use strict';

    // Monitoring state
    let isMonitoring = false;
    let monitorInterval = null;
    let statsInterval = null;
    const MONITOR_INTERVAL_MS = 900;
    const STATS_INTERVAL_MS = 1000;

    // Current subroutine
    let currentSubroutine = 'main';

    // DOM element IDs
    const ELEMENTS = {
        container: 'staticrunglist',
        statusRunning: 'status-running',
        statusScanCount: 'status-scan-count',
        statusScanTime: 'status-scan-time',
        subrSelect: 'subr-select',
        btnMonitor: 'btn-monitor',
        btnStart: 'btn-start',
        btnStop: 'btn-stop',
        btnStep: 'btn-step'
    };

    /**
     * Initialize the monitor
     */
    async function init() {
        // Set up button handlers
        setupButtonHandlers();

        // Load initial program
        await loadProgram(currentSubroutine);

        // Load subroutine list
        await loadSubroutineList();

        // Start stats polling (always on)
        startStatsPolling();
    }

    /**
     * Set up button click handlers
     */
    function setupButtonHandlers() {
        const btnMonitor = document.getElementById(ELEMENTS.btnMonitor);
        const btnStart = document.getElementById(ELEMENTS.btnStart);
        const btnStop = document.getElementById(ELEMENTS.btnStop);
        const btnStep = document.getElementById(ELEMENTS.btnStep);
        const subrSelect = document.getElementById(ELEMENTS.subrSelect);

        if (btnMonitor) {
            btnMonitor.addEventListener('click', toggleMonitoring);
        }

        if (btnStart) {
            btnStart.addEventListener('click', async () => {
                try {
                    await ServerComm.controlStart();
                    updateStats();
                } catch (e) {
                    console.error('Failed to start:', e);
                }
            });
        }

        if (btnStop) {
            btnStop.addEventListener('click', async () => {
                try {
                    await ServerComm.controlStop();
                    updateStats();
                } catch (e) {
                    console.error('Failed to stop:', e);
                }
            });
        }

        if (btnStep) {
            btnStep.addEventListener('click', async () => {
                try {
                    await ServerComm.controlStep();
                    updateStats();
                    if (isMonitoring) {
                        updateData();
                    }
                } catch (e) {
                    console.error('Failed to step:', e);
                }
            });
        }

        if (subrSelect) {
            subrSelect.addEventListener('change', async (e) => {
                currentSubroutine = e.target.value;
                await loadProgram(currentSubroutine);
            });
        }
    }

    /**
     * Load subroutine list and populate selector
     */
    async function loadSubroutineList() {
        try {
            const data = await ServerComm.getSubroutines();
            const select = document.getElementById(ELEMENTS.subrSelect);

            if (select && data.subroutines) {
                select.innerHTML = '';
                data.subroutines.forEach(name => {
                    const option = document.createElement('option');
                    option.value = name;
                    option.textContent = name;
                    if (name === currentSubroutine) {
                        option.selected = true;
                    }
                    select.appendChild(option);
                });
            }
        } catch (e) {
            console.error('Failed to load subroutines:', e);
        }
    }

    /**
     * Load and display a program/subroutine
     * @param {string} subrname - Subroutine name
     */
    async function loadProgram(subrname) {
        const container = document.getElementById(ELEMENTS.container);
        if (container) {
            container.innerHTML = '<div class="loading">Loading program...</div>';
        }

        try {
            const programData = await ServerComm.getProgram(subrname);
            SubrDispControl.renderToContainer(ELEMENTS.container, programData);

            // If monitoring, do an immediate update
            if (isMonitoring) {
                await updateData();
            }
        } catch (e) {
            console.error('Failed to load program:', e);
            if (container) {
                container.innerHTML = `<div class="error-message">Failed to load program: ${e.message}</div>`;
            }
        }
    }

    /**
     * Toggle monitoring on/off
     */
    function toggleMonitoring() {
        if (isMonitoring) {
            stopMonitoring();
        } else {
            startMonitoring();
        }
    }

    /**
     * Start live monitoring
     */
    function startMonitoring() {
        if (isMonitoring) return;

        isMonitoring = true;

        // Update button state
        const btn = document.getElementById(ELEMENTS.btnMonitor);
        if (btn) {
            btn.textContent = 'Stop Monitor';
            btn.classList.add('active');
        }

        // Start polling
        monitorInterval = setInterval(updateData, MONITOR_INTERVAL_MS);

        // Immediate update
        updateData();
    }

    /**
     * Stop live monitoring
     */
    function stopMonitoring() {
        if (!isMonitoring) return;

        isMonitoring = false;

        // Update button state
        const btn = document.getElementById(ELEMENTS.btnMonitor);
        if (btn) {
            btn.textContent = 'Monitor';
            btn.classList.remove('active');
        }

        // Stop polling
        if (monitorInterval) {
            clearInterval(monitorInterval);
            monitorInterval = null;
        }
    }

    /**
     * Start statistics polling
     */
    function startStatsPolling() {
        statsInterval = setInterval(updateStats, STATS_INTERVAL_MS);
        updateStats();
    }

    /**
     * Update statistics display
     */
    async function updateStats() {
        try {
            const stats = await ServerComm.getStatistics();

            const runningEl = document.getElementById(ELEMENTS.statusRunning);
            const scanCountEl = document.getElementById(ELEMENTS.statusScanCount);
            const scanTimeEl = document.getElementById(ELEMENTS.statusScanTime);

            if (runningEl) {
                runningEl.textContent = stats.running ? 'Running' : 'Stopped';
                runningEl.className = 'status-value ' + (stats.running ? 'running' : 'stopped');
            }

            if (scanCountEl) {
                scanCountEl.textContent = stats['scan-count'] || 0;
            }

            if (scanTimeEl) {
                const scanTime = stats['scan-time'] || 0;
                scanTimeEl.textContent = scanTime.toFixed(2) + ' ms';
            }
        } catch (e) {
            console.error('Failed to update stats:', e);
        }
    }

    /**
     * Update data values and cell states
     */
    async function updateData() {
        const addresses = SubrDispControl.getMonitorAddresses();
        if (!addresses || addresses.length === 0) {
            // Debug: log when no addresses are found
            console.debug('LadMonitor: No addresses to monitor');
            return;
        }

        try {
            const data = await ServerComm.getData(addresses);
            if (data && typeof data === 'object') {
                SubrDispControl.updateCellStates(data);
            } else {
                console.warn('LadMonitor: Invalid data received:', data);
            }
        } catch (e) {
            console.error('Failed to update data:', e);
        }
    }

    /**
     * Clean up on page unload
     */
    function cleanup() {
        stopMonitoring();
        if (statsInterval) {
            clearInterval(statsInterval);
            statsInterval = null;
        }
    }

    // Set up cleanup on page unload
    window.addEventListener('beforeunload', cleanup);

    // Public API
    return {
        init,
        loadProgram,
        startMonitoring,
        stopMonitoring,
        toggleMonitoring,
        updateStats,
        updateData
    };
})();

// Initialize when DOM is ready
document.addEventListener('DOMContentLoaded', () => {
    LadMonitor.init();
});
