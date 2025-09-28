/**
 * OSVM CLI Documentation Website
 * Terminal-style interactive functionality
 */

// ===== UTILITY FUNCTIONS =====

/**
 * Copy text to clipboard with visual feedback
 */
function copyToClipboard(text) {
    navigator.clipboard.writeText(text).then(() => {
        // Find the copy button that was clicked
        const copyBtns = document.querySelectorAll('.copy-btn');
        copyBtns.forEach(btn => {
            if (btn.onclick && btn.onclick.toString().includes(text)) {
                const originalText = btn.textContent;
                btn.textContent = 'COPIED!';
                btn.style.background = '#666666';

                setTimeout(() => {
                    btn.textContent = originalText;
                    btn.style.background = '';
                }, 2000);
            }
        });

        showNotification('Copied to clipboard!', 'success');
    }).catch(err => {
        console.error('Failed to copy text: ', err);
        // Fallback for older browsers
        fallbackCopyTextToClipboard(text);
    });
}

/**
 * Copy command to clipboard with enhanced feedback
 */
function copyCommand(command) {
    // Clean up command (remove HTML entities)
    const cleanCommand = command.replace(/&quot;/g, '"').replace(/&amp;/g, '&').replace(/&lt;/g, '<').replace(/&gt;/g, '>');

    navigator.clipboard.writeText(cleanCommand).then(() => {
        // Find the clicked element and add feedback animation
        const clickedElement = event.target.closest('.copyable-command, .command-example');
        if (clickedElement) {
            clickedElement.classList.add('copy-success');
            setTimeout(() => {
                clickedElement.classList.remove('copy-success');
            }, 600);
        }

        // Show notification with command preview
        const shortCommand = cleanCommand.length > 50 ? cleanCommand.substring(0, 47) + '...' : cleanCommand;
        showNotification(`Copied: ${shortCommand}`, 'success');

        // Update copy indicator if present
        const copyIndicator = clickedElement?.querySelector('.copy-indicator');
        if (copyIndicator) {
            const originalText = copyIndicator.textContent;
            copyIndicator.textContent = 'COPIED!';
            copyIndicator.style.color = '#ffffff';

            setTimeout(() => {
                copyIndicator.textContent = originalText;
                copyIndicator.style.color = '';
            }, 1500);
        }

    }).catch(err => {
        console.error('Failed to copy command: ', err);
        fallbackCopyTextToClipboard(cleanCommand);
        showNotification('Copy failed - please try again', 'error');
    });
}

/**
 * Fallback copy function for older browsers
 */
function fallbackCopyTextToClipboard(text) {
    const textArea = document.createElement('textarea');
    textArea.value = text;
    textArea.style.top = '0';
    textArea.style.left = '0';
    textArea.style.position = 'fixed';

    document.body.appendChild(textArea);
    textArea.focus();
    textArea.select();

    try {
        const successful = document.execCommand('copy');
        if (successful) {
            showNotification('Copied to clipboard!', 'success');
        } else {
            showNotification('Copy failed', 'error');
        }
    } catch (err) {
        showNotification('Copy not supported', 'error');
    }

    document.body.removeChild(textArea);
}

/**
 * Show notification message
 */
function showNotification(message, type = 'info') {
    // Remove existing notifications
    const existingNotifications = document.querySelectorAll('.notification');
    existingNotifications.forEach(notification => {
        notification.remove();
    });

    // Create notification element
    const notification = document.createElement('div');
    notification.className = `notification notification-${type}`;
    notification.textContent = message;

    // Style the notification
    Object.assign(notification.style, {
        position: 'fixed',
        top: '20px',
        right: '20px',
        background: type === 'success' ? '#666666' : type === 'error' ? '#444444' : '#333333',
        color: '#e0e0e0',
        padding: '12px 20px',
        borderRadius: '4px',
        fontFamily: 'JetBrains Mono, monospace',
        fontSize: '0.875rem',
        fontWeight: '500',
        border: '1px solid #999999',
        zIndex: '10000',
        animation: 'slideInRight 0.3s ease-out',
        maxWidth: '300px',
        wordWrap: 'break-word'
    });

    // Add animation styles
    const style = document.createElement('style');
    style.textContent = `
        @keyframes slideInRight {
            from {
                transform: translateX(100%);
                opacity: 0;
            }
            to {
                transform: translateX(0);
                opacity: 1;
            }
        }
        @keyframes slideOutRight {
            from {
                transform: translateX(0);
                opacity: 1;
            }
            to {
                transform: translateX(100%);
                opacity: 0;
            }
        }
    `;
    document.head.appendChild(style);

    document.body.appendChild(notification);

    // Auto-remove after 3 seconds
    setTimeout(() => {
        notification.style.animation = 'slideOutRight 0.3s ease-in forwards';
        setTimeout(() => {
            if (notification.parentNode) {
                notification.parentNode.removeChild(notification);
            }
        }, 300);
    }, 3000);
}

// ===== TAB FUNCTIONALITY =====

/**
 * Show specific tab content
 */
function showTab(tabName) {
    // Hide all tab contents
    const tabContents = document.querySelectorAll('.tab-content');
    tabContents.forEach(content => {
        content.classList.remove('active');
    });

    // Remove active class from all tab buttons
    const tabBtns = document.querySelectorAll('.tab-btn');
    tabBtns.forEach(btn => {
        btn.classList.remove('active');
    });

    // Show selected tab content
    const selectedTab = document.getElementById(tabName);
    if (selectedTab) {
        selectedTab.classList.add('active');
    }

    // Add active class to clicked button
    const clickedBtn = event?.target;
    if (clickedBtn && clickedBtn.classList.contains('tab-btn')) {
        clickedBtn.classList.add('active');
    } else {
        // Fallback: find button by text content
        tabBtns.forEach(btn => {
            if (btn.textContent.toLowerCase() === tabName.toUpperCase()) {
                btn.classList.add('active');
            }
        });
    }
}

// ===== SMOOTH SCROLLING =====

/**
 * Smooth scroll to anchor links
 */
function initSmoothScrolling() {
    document.querySelectorAll('a[href^="#"]').forEach(anchor => {
        anchor.addEventListener('click', function (e) {
            e.preventDefault();
            const target = document.querySelector(this.getAttribute('href'));
            if (target) {
                target.scrollIntoView({
                    behavior: 'smooth',
                    block: 'start'
                });
            }
        });
    });
}

// ===== TERMINAL EFFECTS =====

/**
 * Add typing effect to command elements
 */
function initTypingEffects() {
    const typingElements = document.querySelectorAll('.typing');

    typingElements.forEach(element => {
        const text = element.textContent;
        element.textContent = '';

        let i = 0;
        const timer = setInterval(() => {
            if (i < text.length) {
                element.textContent += text.charAt(i);
                i++;
            } else {
                clearInterval(timer);
                // Remove typing class to stop cursor blinking
                setTimeout(() => {
                    element.classList.remove('typing');
                }, 1000);
            }
        }, 100);
    });
}

/**
 * Add matrix-style background effect (subtle)
 */
function initMatrixEffect() {
    // Create matrix rain effect in background (very subtle)
    const canvas = document.createElement('canvas');
    const ctx = canvas.getContext('2d');

    canvas.style.position = 'fixed';
    canvas.style.top = '0';
    canvas.style.left = '0';
    canvas.style.width = '100%';
    canvas.style.height = '100%';
    canvas.style.zIndex = '-1';
    canvas.style.opacity = '0.03';
    canvas.style.pointerEvents = 'none';

    document.body.appendChild(canvas);

    function resizeCanvas() {
        canvas.width = window.innerWidth;
        canvas.height = window.innerHeight;
    }

    resizeCanvas();
    window.addEventListener('resize', resizeCanvas);

    const matrix = '01';
    const matrixArray = matrix.split('');

    const fontSize = 14;
    const columns = canvas.width / fontSize;

    const drops = [];
    for (let x = 0; x < columns; x++) {
        drops[x] = 1;
    }

    function draw() {
        ctx.fillStyle = 'rgba(0, 0, 0, 0.04)';
        ctx.fillRect(0, 0, canvas.width, canvas.height);

        ctx.fillStyle = '#333';
        ctx.font = fontSize + 'px monospace';

        for (let i = 0; i < drops.length; i++) {
            const text = matrixArray[Math.floor(Math.random() * matrixArray.length)];
            ctx.fillText(text, i * fontSize, drops[i] * fontSize);

            if (drops[i] * fontSize > canvas.height && Math.random() > 0.975) {
                drops[i] = 0;
            }

            drops[i]++;
        }
    }

    setInterval(draw, 100);
}

// ===== KEYBOARD NAVIGATION =====

/**
 * Add keyboard navigation support
 */
function initKeyboardNavigation() {
    document.addEventListener('keydown', (e) => {
        // Tab navigation for accessibility
        if (e.key === 'Tab') {
            document.body.classList.add('keyboard-nav');
        }

        // Escape key to close any modals or reset focus
        if (e.key === 'Escape') {
            document.activeElement.blur();
        }

        // Arrow key navigation for tabs
        if (e.key === 'ArrowLeft' || e.key === 'ArrowRight') {
            const activeTab = document.querySelector('.tab-btn.active');
            if (activeTab) {
                const tabs = Array.from(document.querySelectorAll('.tab-btn'));
                const currentIndex = tabs.indexOf(activeTab);
                let newIndex;

                if (e.key === 'ArrowLeft') {
                    newIndex = currentIndex > 0 ? currentIndex - 1 : tabs.length - 1;
                } else {
                    newIndex = currentIndex < tabs.length - 1 ? currentIndex + 1 : 0;
                }

                tabs[newIndex].click();
                tabs[newIndex].focus();
                e.preventDefault();
            }
        }
    });

    // Remove keyboard navigation class on mouse use
    document.addEventListener('mousedown', () => {
        document.body.classList.remove('keyboard-nav');
    });
}

// ===== PERFORMANCE OPTIMIZATIONS =====

/**
 * Lazy load images and content
 */
function initLazyLoading() {
    if ('IntersectionObserver' in window) {
        const lazyImages = document.querySelectorAll('img[data-src]');

        const imageObserver = new IntersectionObserver((entries, observer) => {
            entries.forEach(entry => {
                if (entry.isIntersecting) {
                    const img = entry.target;
                    img.src = img.dataset.src;
                    img.classList.remove('lazy');
                    imageObserver.unobserve(img);
                }
            });
        });

        lazyImages.forEach(img => imageObserver.observe(img));
    }
}

/**
 * Throttle scroll events for performance
 */
function throttle(func, wait) {
    let timeout;
    return function executedFunction(...args) {
        const later = () => {
            clearTimeout(timeout);
            func(...args);
        };
        clearTimeout(timeout);
        timeout = setTimeout(later, wait);
    };
}

// ===== MOBILE OPTIMIZATIONS =====

/**
 * Handle mobile-specific interactions
 */
function initMobileOptimizations() {
    // Prevent zoom on double tap for iOS
    let lastTouchEnd = 0;
    document.addEventListener('touchend', (event) => {
        const now = (new Date()).getTime();
        if (now - lastTouchEnd <= 300) {
            event.preventDefault();
        }
        lastTouchEnd = now;
    }, false);

    // Add touch feedback for buttons
    const buttons = document.querySelectorAll('button, .tab-btn, .copy-btn');
    buttons.forEach(button => {
        button.addEventListener('touchstart', () => {
            button.style.transform = 'scale(0.95)';
        });

        button.addEventListener('touchend', () => {
            button.style.transform = '';
        });
    });
}

// ===== ACCESSIBILITY ENHANCEMENTS =====

/**
 * Enhanced accessibility features
 */
function initAccessibility() {
    // Add aria labels to interactive elements
    const copyBtns = document.querySelectorAll('.copy-btn');
    copyBtns.forEach(btn => {
        btn.setAttribute('aria-label', 'Copy command to clipboard');
        btn.setAttribute('role', 'button');
    });

    // Add keyboard support for custom buttons
    const customBtns = document.querySelectorAll('[role="button"]:not(button)');
    customBtns.forEach(btn => {
        btn.setAttribute('tabindex', '0');
        btn.addEventListener('keydown', (e) => {
            if (e.key === 'Enter' || e.key === ' ') {
                e.preventDefault();
                btn.click();
            }
        });
    });

    // Announce page load for screen readers
    const announcement = document.createElement('div');
    announcement.setAttribute('aria-live', 'polite');
    announcement.setAttribute('aria-atomic', 'true');
    announcement.className = 'sr-only';
    announcement.textContent = 'OSVM CLI documentation page loaded';
    document.body.appendChild(announcement);
}

// ===== EASTER EGGS =====

/**
 * Add some fun terminal-style easter eggs
 */
function initEasterEggs() {
    // Konami code easter egg
    let konamiCode = [];
    const konamiSequence = [
        'ArrowUp', 'ArrowUp', 'ArrowDown', 'ArrowDown',
        'ArrowLeft', 'ArrowRight', 'ArrowLeft', 'ArrowRight',
        'KeyB', 'KeyA'
    ];

    document.addEventListener('keydown', (e) => {
        konamiCode.push(e.code);

        if (konamiCode.length > konamiSequence.length) {
            konamiCode.shift();
        }

        if (konamiCode.join(',') === konamiSequence.join(',')) {
            showNotification('🎮 KONAMI CODE ACTIVATED! Welcome, hacker!', 'success');
            // Add temporary matrix effect
            document.body.style.animation = 'matrix-glow 3s ease-in-out';
            setTimeout(() => {
                document.body.style.animation = '';
            }, 3000);
        }
    });
}

// ===== INITIALIZATION =====

/**
 * Initialize all functionality when DOM is loaded
 */
document.addEventListener('DOMContentLoaded', () => {
    console.log('🖥️  OSVM CLI Documentation System Online');
    console.log('📡 Initializing terminal interface...');

    try {
        initSmoothScrolling();
        initKeyboardNavigation();
        initLazyLoading();
        initMobileOptimizations();
        initAccessibility();
        initEasterEggs();

        // Add a small delay for visual effect
        setTimeout(() => {
            initTypingEffects();
        }, 500);

        // Initialize matrix effect only on larger screens
        if (window.innerWidth > 768) {
            initMatrixEffect();
        }

        console.log('✅ Terminal interface initialized successfully');

    } catch (error) {
        console.error('❌ Error initializing terminal interface:', error);
    }
});

// ===== GLOBAL FUNCTIONS =====

// Make functions available globally for HTML onclick handlers
window.copyToClipboard = copyToClipboard;
window.copyCommand = copyCommand;
window.showTab = showTab;

// Add CSS for screen reader only content
const style = document.createElement('style');
style.textContent = `
    .sr-only {
        position: absolute;
        width: 1px;
        height: 1px;
        padding: 0;
        margin: -1px;
        overflow: hidden;
        clip: rect(0, 0, 0, 0);
        white-space: nowrap;
        border: 0;
    }

    .keyboard-nav *:focus {
        outline: 2px solid #ffffff !important;
        outline-offset: 2px !important;
    }

    @keyframes matrix-glow {
        0% { filter: hue-rotate(0deg) brightness(1); }
        50% { filter: hue-rotate(180deg) brightness(1.2); }
        100% { filter: hue-rotate(360deg) brightness(1); }
    }
`;
document.head.appendChild(style);