/**
 * Dynamic Search System with Ctrl+K Support
 * Provides instant search across all documentation pages
 */

// ===== SEARCH STATE =====
let selectedSearchResult = 0;

/**
 * Setup search system
 */
function setupSearchSystem() {
    createSearchModal();
    console.log('🔍 Search system initialized');
}

/**
 * Create search modal HTML
 */
function createSearchModal() {
    const searchModalHTML = `
        <div id="search-modal" class="search-modal" style="display: none;">
            <div class="search-backdrop" onclick="closeSearch()"></div>
            <div class="search-container">
                <div class="search-header">
                    <div class="search-input-wrapper">
                        <span class="search-icon">🔍</span>
                        <input type="text" id="search-input" placeholder="Search documentation..." autocomplete="off">
                        <span class="search-shortcut">ESC</span>
                    </div>
                </div>
                <div class="search-results" id="search-results">
                    <div class="search-empty">
                        <span class="search-empty-icon">📖</span>
                        <p>Start typing to search documentation...</p>
                        <div class="search-tips">
                            <span class="tip">Try: "plugin", "theme", "AI", "deployment"</span>
                        </div>
                    </div>
                </div>
                <div class="search-footer">
                    <div class="search-footer-hints">
                        <span class="hint"><kbd>↑↓</kbd> Navigate</span>
                        <span class="hint"><kbd>Enter</kbd> Open</span>
                        <span class="hint"><kbd>Esc</kbd> Close</span>
                    </div>
                </div>
            </div>
        </div>
    `;

    // Add to document
    document.body.insertAdjacentHTML('beforeend', searchModalHTML);

    // Setup search input event listeners
    const searchInput = document.getElementById('search-input');
    searchInput.addEventListener('input', handleSearchInput);
    searchInput.addEventListener('keydown', handleSearchKeyboard);
}

/**
 * Toggle search modal
 */
function toggleSearch() {
    if (isSearchOpen) {
        closeSearch();
    } else {
        openSearch();
    }
}

/**
 * Open search modal
 */
function openSearch() {
    const modal = document.getElementById('search-modal');
    const input = document.getElementById('search-input');

    if (modal && input) {
        isSearchOpen = true;
        modal.style.display = 'flex';

        // Animate in
        requestAnimationFrame(() => {
            modal.classList.add('search-modal-open');
        });

        // Focus input
        setTimeout(() => {
            input.focus();
        }, 150);

        // Add body class to prevent scrolling
        document.body.classList.add('search-open');

        console.log('🔍 Search opened');
    }
}

/**
 * Close search modal
 */
function closeSearch() {
    const modal = document.getElementById('search-modal');
    const input = document.getElementById('search-input');

    if (modal && input) {
        isSearchOpen = false;
        modal.classList.remove('search-modal-open');

        // Animate out
        setTimeout(() => {
            modal.style.display = 'none';
            input.value = '';
            clearSearchResults();
        }, 200);

        // Remove body class
        document.body.classList.remove('search-open');

        console.log('🔍 Search closed');
    }
}

/**
 * Handle search input
 */
function handleSearchInput(e) {
    const query = e.target.value.trim();

    if (query.length === 0) {
        showEmptySearch();
        return;
    }

    if (query.length < 2) {
        return;
    }

    // Perform search
    const results = performSearch(query);
    displaySearchResults(results, query);
}

/**
 * Handle search keyboard navigation
 */
function handleSearchKeyboard(e) {
    const resultsContainer = document.getElementById('search-results');
    const results = resultsContainer.querySelectorAll('.search-result-item');

    switch (e.key) {
        case 'ArrowDown':
            e.preventDefault();
            selectedSearchResult = Math.min(selectedSearchResult + 1, results.length - 1);
            updateSearchSelection(results);
            break;

        case 'ArrowUp':
            e.preventDefault();
            selectedSearchResult = Math.max(selectedSearchResult - 1, 0);
            updateSearchSelection(results);
            break;

        case 'Enter':
            e.preventDefault();
            const selectedResult = results[selectedSearchResult];
            if (selectedResult) {
                const pageId = selectedResult.dataset.page;
                closeSearch();
                navigateToPage(pageId);
            }
            break;

        case 'Escape':
            e.preventDefault();
            closeSearch();
            break;
    }
}

/**
 * Perform fuzzy search across indexed content
 */
function performSearch(query) {
    const normalizedQuery = query.toLowerCase();
    const results = [];

    for (const entry of searchIndex) {
        const score = calculateSearchScore(entry, normalizedQuery);
        if (score > 0) {
            results.push({
                ...entry,
                searchScore: score
            });
        }
    }

    // Sort by relevance and type priority
    results.sort((a, b) => {
        // Type priority: page > heading > command > content
        const typePriority = { page: 4, heading: 3, command: 2, content: 1 };
        const aPriority = typePriority[a.type] || 0;
        const bPriority = typePriority[b.type] || 0;

        if (aPriority !== bPriority) {
            return bPriority - aPriority;
        }

        return b.searchScore - a.searchScore;
    });

    return results.slice(0, 10); // Limit to top 10 results
}

/**
 * Calculate search relevance score
 */
function calculateSearchScore(entry, query) {
    const title = entry.title.toLowerCase();
    const content = entry.content.toLowerCase();

    let score = 0;

    // Exact title match
    if (title === query) {
        score += 100;
    }

    // Title starts with query
    if (title.startsWith(query)) {
        score += 80;
    }

    // Title contains query
    if (title.includes(query)) {
        score += 60;
    }

    // Content contains query
    if (content.includes(query)) {
        score += 40;
    }

    // Fuzzy matching for partial matches
    const titleFuzzyScore = calculateFuzzyScore(title, query);
    const contentFuzzyScore = calculateFuzzyScore(content, query);

    score += titleFuzzyScore * 2; // Title matches are more important
    score += contentFuzzyScore;

    // Boost score based on entry type and base score
    score += entry.score || 0;

    return score;
}

/**
 * Calculate fuzzy matching score
 */
function calculateFuzzyScore(text, query) {
    if (!text || !query) return 0;

    const textChars = text.split('');
    const queryChars = query.split('');

    let score = 0;
    let queryIndex = 0;
    let consecutiveMatches = 0;

    for (let i = 0; i < textChars.length && queryIndex < queryChars.length; i++) {
        if (textChars[i] === queryChars[queryIndex]) {
            score += 1 + consecutiveMatches;
            consecutiveMatches++;
            queryIndex++;
        } else {
            consecutiveMatches = 0;
        }
    }

    // Bonus for completing the query
    if (queryIndex === queryChars.length) {
        score += 20;
    }

    return score;
}

/**
 * Display search results
 */
function displaySearchResults(results, query) {
    const container = document.getElementById('search-results');
    selectedSearchResult = 0;

    if (results.length === 0) {
        container.innerHTML = `
            <div class="search-empty">
                <span class="search-empty-icon">🤷‍♂️</span>
                <p>No results found for "${query}"</p>
                <div class="search-tips">
                    <span class="tip">Try different keywords or check spelling</span>
                </div>
            </div>
        `;
        return;
    }

    const resultsHTML = results.map((result, index) => {
        const pageTitle = pages[result.page]?.title || result.page;
        const typeIcon = getResultTypeIcon(result.type);
        const highlightedTitle = highlightSearchTerms(result.title, query);
        const highlightedContent = highlightSearchTerms(result.content, query);

        return `
            <div class="search-result-item ${index === 0 ? 'selected' : ''}"
                 data-page="${result.page}"
                 onclick="selectSearchResult('${result.page}')">
                <div class="search-result-icon">${typeIcon}</div>
                <div class="search-result-content">
                    <div class="search-result-title">${highlightedTitle}</div>
                    <div class="search-result-description">${highlightedContent}</div>
                    <div class="search-result-page">${pageTitle}</div>
                </div>
                <div class="search-result-type">${result.type}</div>
            </div>
        `;
    }).join('');

    container.innerHTML = resultsHTML;
}

/**
 * Show empty search state
 */
function showEmptySearch() {
    const container = document.getElementById('search-results');
    container.innerHTML = `
        <div class="search-empty">
            <span class="search-empty-icon">📖</span>
            <p>Start typing to search documentation...</p>
            <div class="search-tips">
                <span class="tip">Try: "plugin", "theme", "AI", "deployment"</span>
            </div>
        </div>
    `;
}

/**
 * Clear search results
 */
function clearSearchResults() {
    selectedSearchResult = 0;
    showEmptySearch();
}

/**
 * Update search result selection
 */
function updateSearchSelection(results) {
    results.forEach((result, index) => {
        result.classList.toggle('selected', index === selectedSearchResult);
    });

    // Scroll selected result into view
    const selectedResult = results[selectedSearchResult];
    if (selectedResult) {
        selectedResult.scrollIntoView({
            behavior: 'smooth',
            block: 'nearest'
        });
    }
}

/**
 * Select search result and navigate
 */
function selectSearchResult(pageId) {
    closeSearch();
    navigateToPage(pageId);
}

/**
 * Get icon for result type
 */
function getResultTypeIcon(type) {
    const icons = {
        page: '📄',
        heading: '📝',
        command: '⌨️',
        content: '📋'
    };
    return icons[type] || '📋';
}

/**
 * Highlight search terms in text
 */
function highlightSearchTerms(text, query) {
    if (!query || !text) return text;

    const regex = new RegExp(`(${escapeRegex(query)})`, 'gi');
    return text.replace(regex, '<mark>$1</mark>');
}

/**
 * Escape regex special characters
 */
function escapeRegex(string) {
    return string.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

// Make search functions available globally
window.toggleSearch = toggleSearch;
window.openSearch = openSearch;
window.closeSearch = closeSearch;
window.setupSearchSystem = setupSearchSystem;
window.selectSearchResult = selectSearchResult;