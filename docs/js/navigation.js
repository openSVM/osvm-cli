/**
 * Multi-page Navigation System with Search
 * Handles page transitions, search functionality, and keyboard navigation
 */

// ===== GLOBAL STATE =====
let currentPage = 'home';
let searchIndex = [];
let isSearchOpen = false;
let searchResults = [];

// ===== PAGE ROUTING =====
const pages = {
    'home': {
        title: 'OSVM CLI - Home',
        file: 'pages/home.html',
        keywords: ['home', 'overview', 'introduction', 'getting started']
    },
    'installation': {
        title: 'Installation Guide',
        file: 'pages/installation.html',
        keywords: ['install', 'setup', 'download', 'requirements']
    },
    'plugins': {
        title: 'Plugin System',
        file: 'pages/plugins.html',
        keywords: ['plugins', 'extensions', 'commands', 'tools', 'themes']
    },
    'themes': {
        title: 'Themes & Customization',
        file: 'pages/themes.html',
        keywords: ['themes', 'styling', 'customization', 'colors', 'appearance']
    },
    'ai-integration': {
        title: 'AI Integration',
        file: 'pages/ai-integration.html',
        keywords: ['ai', 'artificial intelligence', 'analysis', 'security', 'audit']
    },
    'mcp-servers': {
        title: 'MCP Servers',
        file: 'pages/mcp-servers.html',
        keywords: ['mcp', 'model context protocol', 'servers', 'blockchain', 'data']
    },
    'node-deployment': {
        title: 'Node Deployment',
        file: 'pages/node-deployment.html',
        keywords: ['deployment', 'nodes', 'validators', 'rpc', 'ssh']
    },
    'api-reference': {
        title: 'API Reference',
        file: 'pages/api-reference.html',
        keywords: ['api', 'reference', 'commands', 'documentation']
    },
    'isolation': {
        title: 'Isolation & Security',
        file: 'pages/isolation.html',
        keywords: ['isolation', 'security', 'microvm', 'firecracker', 'ephemeral']
    },
    'ovsm-language': {
        title: 'OVSM Language',
        file: 'pages/ovsm-language.html',
        keywords: ['ovsm', 'language', 'planning', 'agent', 'research', 'multi-agent', 'tools', 'specification']
    }
};

/**
 * Initialize navigation system
 */
function initNavigation() {
    // Build search index
    buildSearchIndex();

    // Setup keyboard shortcuts
    setupKeyboardShortcuts();

    // Setup navigation links
    setupNavigationLinks();

    // Setup search functionality
    setupSearchSystem();

    // Load initial page
    const initialPage = getPageFromURL() || 'home';
    navigateToPage(initialPage, false);

    console.log('🧭 Navigation system initialized');
}

/**
 * Navigate to a specific page with smooth transition
 */
async function navigateToPage(pageId, addToHistory = true) {
    if (!pages[pageId]) {
        console.error(`Page not found: ${pageId}`);
        return;
    }

    const page = pages[pageId];

    // Update URL if needed
    if (addToHistory) {
        const newURL = pageId === 'home' ? '/' : `/${pageId}`;
        history.pushState({ page: pageId }, page.title, newURL);
    }

    // Update document title
    document.title = page.title;

    // Start page transition
    await startPageTransition();

    try {
        // Load page content
        const content = await loadPageContent(page.file);

        // Update content
        updatePageContent(content);

        // Update navigation state
        updateNavigationState(pageId);

        // Complete transition
        await completePageTransition();

        // Update current page
        currentPage = pageId;

        // Process cross-links for the new page
        processPageCrossLinks();

        console.log(`📄 Navigated to page: ${pageId}`);

    } catch (error) {
        console.error('Failed to load page:', error);
        showErrorPage();
    }
}

/**
 * Start page transition animation
 */
async function startPageTransition() {
    const content = document.querySelector('.page-content');
    if (content) {
        content.classList.add('page-transition-out');
        await new Promise(resolve => setTimeout(resolve, 150));
    }
}

/**
 * Complete page transition animation
 */
async function completePageTransition() {
    const content = document.querySelector('.page-content');
    if (content) {
        content.classList.remove('page-transition-out');
        content.classList.add('page-transition-in');

        // Remove transition class after animation
        setTimeout(() => {
            content.classList.remove('page-transition-in');
        }, 300);
    }
}

/**
 * Load page content from file
 */
async function loadPageContent(filePath) {
    const response = await fetch(filePath);
    if (!response.ok) {
        throw new Error(`HTTP ${response.status}: ${response.statusText}`);
    }
    return await response.text();
}

/**
 * Update page content
 */
function updatePageContent(content) {
    const contentContainer = document.querySelector('.page-content');
    if (contentContainer) {
        contentContainer.innerHTML = content;

        // Reinitialize any interactive elements
        initializePageElements();
    }
}

/**
 * Update navigation state
 */
function updateNavigationState(activePageId) {
    // Update active nav link
    document.querySelectorAll('.nav-link').forEach(link => {
        link.classList.remove('active');
        if (link.dataset.page === activePageId) {
            link.classList.add('active');
        }
    });

    // Update breadcrumbs if present
    updateBreadcrumbs(activePageId);
}

/**
 * Get page ID from current URL
 */
function getPageFromURL() {
    const path = window.location.pathname;
    if (path === '/' || path === '/index.html') {
        return 'home';
    }

    const pageId = path.replace(/^\//, '').replace(/\.html$/, '');
    return pages[pageId] ? pageId : null;
}

/**
 * Setup keyboard shortcuts
 */
function setupKeyboardShortcuts() {
    document.addEventListener('keydown', (e) => {
        // Ctrl+K or Cmd+K for search
        if ((e.ctrlKey || e.metaKey) && e.key === 'k') {
            e.preventDefault();
            toggleSearch();
            return;
        }

        // Escape to close search
        if (e.key === 'Escape' && isSearchOpen) {
            closeSearch();
            return;
        }

        // Handle search navigation
        if (isSearchOpen) {
            handleSearchKeyboard(e);
            return;
        }

        // Page navigation shortcuts
        if (e.altKey) {
            switch (e.key) {
                case '1':
                    e.preventDefault();
                    navigateToPage('home');
                    break;
                case '2':
                    e.preventDefault();
                    navigateToPage('installation');
                    break;
                case '3':
                    e.preventDefault();
                    navigateToPage('plugins');
                    break;
                case '4':
                    e.preventDefault();
                    navigateToPage('themes');
                    break;
            }
        }
    });
}

/**
 * Setup navigation links
 */
function setupNavigationLinks() {
    // Handle navigation clicks
    document.addEventListener('click', (e) => {
        const navLink = e.target.closest('[data-page]');
        if (navLink) {
            e.preventDefault();
            const pageId = navLink.dataset.page;
            navigateToPage(pageId);
        }

        // Handle cross-links
        const crossLink = e.target.closest('[data-cross-link]');
        if (crossLink) {
            e.preventDefault();
            const pageId = crossLink.dataset.crossLink;
            navigateToPage(pageId);
        }
    });

    // Handle browser back/forward
    window.addEventListener('popstate', (e) => {
        const pageId = e.state?.page || getPageFromURL() || 'home';
        navigateToPage(pageId, false);
    });
}

/**
 * Build search index from all pages
 */
async function buildSearchIndex() {
    searchIndex = [];

    for (const [pageId, page] of Object.entries(pages)) {
        try {
            const content = await loadPageContent(page.file);

            // Extract text content (simple approach)
            const tempDiv = document.createElement('div');
            tempDiv.innerHTML = content;
            const textContent = tempDiv.textContent || tempDiv.innerText || '';

            // Create search entries
            const entries = extractSearchEntries(textContent, pageId, page);
            searchIndex.push(...entries);

        } catch (error) {
            console.warn(`Failed to index page ${pageId}:`, error);
        }
    }

    console.log(`🔍 Search index built with ${searchIndex.length} entries`);
}

/**
 * Extract search entries from page content
 */
function extractSearchEntries(content, pageId, page) {
    const entries = [];

    // Add page title and keywords
    entries.push({
        title: page.title,
        content: page.keywords.join(' '),
        page: pageId,
        type: 'page',
        score: 100
    });

    // Extract headings and commands
    const lines = content.split('\n');
    let currentSection = '';

    for (let i = 0; i < lines.length; i++) {
        const line = lines[i].trim();

        // Check for headings
        const headingMatch = line.match(/^#+\s+(.+)$/);
        if (headingMatch) {
            currentSection = headingMatch[1];
            entries.push({
                title: currentSection,
                content: line,
                page: pageId,
                type: 'heading',
                score: 80
            });
            continue;
        }

        // Check for commands
        const commandMatch = line.match(/\$\s+(osvm\s+.+?)(?:\s*#|$)/);
        if (commandMatch) {
            entries.push({
                title: commandMatch[1],
                content: `${currentSection} - ${line}`,
                page: pageId,
                type: 'command',
                score: 90
            });
            continue;
        }

        // Check for important terms
        if (line.length > 20 && line.length < 200) {
            entries.push({
                title: line.substring(0, 50) + (line.length > 50 ? '...' : ''),
                content: line,
                page: pageId,
                type: 'content',
                score: 50
            });
        }
    }

    return entries;
}

/**
 * Initialize interactive elements on the current page
 */
function initializePageElements() {
    // Reinitialize copy buttons
    if (window.initCopyButtons) {
        window.initCopyButtons();
    }

    // Reinitialize expandable commands
    if (window.initExpandableCommands) {
        window.initExpandableCommands();
    }

    // Reinitialize copy to clipboard
    if (window.initCopyToClipboard) {
        window.initCopyToClipboard();
    }

    // Reinitialize any other interactive elements
    if (window.initTabs) {
        window.initTabs();
    }

    // Add micro-animations to new elements
    addMicroAnimations();
}

/**
 * Add micro-animations to page elements
 */
function addMicroAnimations() {
    // Animate elements that come into view
    const animatedElements = document.querySelectorAll('.command-example, .feature-card, .code-block');

    const observer = new IntersectionObserver((entries) => {
        entries.forEach(entry => {
            if (entry.isIntersecting) {
                entry.target.classList.add('animate-in');
                observer.unobserve(entry.target);
            }
        });
    }, {
        threshold: 0.1,
        rootMargin: '50px'
    });

    animatedElements.forEach(el => {
        el.classList.add('animate-ready');
        observer.observe(el);
    });
}

/**
 * Process cross-links in the current page
 */
function processPageCrossLinks() {
    const content = document.querySelector('.page-content');
    if (!content) return;

    // Define cross-link patterns
    const crossLinks = {
        'plugin': 'plugins',
        'plugins': 'plugins',
        'theme': 'themes',
        'themes': 'themes',
        'AI': 'ai-integration',
        'artificial intelligence': 'ai-integration',
        'MCP': 'mcp-servers',
        'Model Context Protocol': 'mcp-servers',
        'deployment': 'node-deployment',
        'install': 'installation',
        'API': 'api-reference'
    };

    // Process text nodes to add cross-links
    const walker = document.createTreeWalker(
        content,
        NodeFilter.SHOW_TEXT,
        null,
        false
    );

    const textNodes = [];
    let node;
    while (node = walker.nextNode()) {
        if (node.parentElement.tagName !== 'A' &&
            node.parentElement.tagName !== 'CODE' &&
            node.parentElement.tagName !== 'PRE') {
            textNodes.push(node);
        }
    }

    textNodes.forEach(textNode => {
        let content = textNode.textContent;
        let hasChanges = false;

        for (const [term, pageId] of Object.entries(crossLinks)) {
            if (pageId !== currentPage && content.includes(term)) {
                const regex = new RegExp(`\\b${term}\\b`, 'gi');
                content = content.replace(regex, `<a href="#" data-cross-link="${pageId}" class="cross-link">$&</a>`);
                hasChanges = true;
            }
        }

        if (hasChanges) {
            const wrapper = document.createElement('span');
            wrapper.innerHTML = content;
            textNode.parentNode.replaceChild(wrapper, textNode);
        }
    });
}

// Make navigation functions available globally
window.navigateToPage = navigateToPage;
window.initNavigation = initNavigation;