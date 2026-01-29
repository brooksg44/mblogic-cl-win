/**
 * servercomm.js - Server Communication Module
 * Handles all API communication with the MBLogic-CL web server
 */

const ServerComm = (function() {
    'use strict';

    // Base URL for API endpoints
    const API_BASE = '';

    /**
     * Make an API request
     * @param {string} endpoint - API endpoint path
     * @param {Object} options - fetch options
     * @returns {Promise<Object>} - JSON response
     */
    async function apiRequest(endpoint, options = {}) {
        try {
            const response = await fetch(API_BASE + endpoint, {
                ...options,
                headers: {
                    'Content-Type': 'application/json',
                    ...options.headers
                }
            });

            if (!response.ok) {
                throw new Error(`HTTP error! status: ${response.status}`);
            }

            return await response.json();
        } catch (error) {
            console.error(`API request failed: ${endpoint}`, error);
            throw error;
        }
    }

    /**
     * Get PLC statistics
     * @returns {Promise<Object>} - Statistics object
     */
    async function getStatistics() {
        return apiRequest('/api/statistics');
    }

    /**
     * Get data values for specified addresses
     * @param {string[]} addresses - Array of address strings
     * @returns {Promise<Object>} - Object mapping addresses to values
     */
    async function getData(addresses) {
        if (!addresses || addresses.length === 0) {
            return {};
        }
        const addrParam = addresses.join(',');
        return apiRequest(`/api/data?addr=${encodeURIComponent(addrParam)}`);
    }

    /**
     * Get program structure for a subroutine
     * @param {string} subrname - Subroutine name (default: 'main')
     * @returns {Promise<Object>} - Program structure with ladder data
     */
    async function getProgram(subrname = 'main') {
        return apiRequest(`/api/program?subrname=${encodeURIComponent(subrname)}`);
    }

    /**
     * Get list of available subroutines
     * @returns {Promise<Object>} - Object with subroutines array
     */
    async function getSubroutines() {
        return apiRequest('/api/subroutines');
    }

    /**
     * Start PLC execution
     * @returns {Promise<Object>} - Control response
     */
    async function controlStart() {
        return apiRequest('/api/control/start', { method: 'POST' });
    }

    /**
     * Stop PLC execution
     * @returns {Promise<Object>} - Control response
     */
    async function controlStop() {
        return apiRequest('/api/control/stop', { method: 'POST' });
    }

    /**
     * Execute single scan step
     * @returns {Promise<Object>} - Control response
     */
    async function controlStep() {
        return apiRequest('/api/control/step', { method: 'POST' });
    }

    // Public API
    return {
        getStatistics,
        getData,
        getProgram,
        getSubroutines,
        controlStart,
        controlStop,
        controlStep
    };
})();
