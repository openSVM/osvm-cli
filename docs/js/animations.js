/**
 * Animation System for Multi-page Website
 * Handles micro-animations, transitions, and visual effects
 */

// ===== ANIMATION UTILITIES =====

/**
 * Initialize all animation systems
 */
function initAnimations() {
    setupScrollAnimations();
    setupHoverEffects();
    setupLoadingAnimations();
    setupParticleEffects();
    setupTypingAnimations();

    console.log('✨ Animation system initialized');
}

/**
 * Setup scroll-triggered animations
 */
function setupScrollAnimations() {
    // Intersection Observer for scroll animations
    const animationObserver = new IntersectionObserver((entries) => {
        entries.forEach(entry => {
            if (entry.isIntersecting) {
                const element = entry.target;
                const animationType = element.dataset.animation || 'fadeIn';
                const delay = parseInt(element.dataset.delay) || 0;

                setTimeout(() => {
                    element.classList.add('animate-in');
                    element.classList.add(`animate-${animationType}`);
                }, delay);

                animationObserver.unobserve(element);
            }
        });
    }, {
        threshold: 0.1,
        rootMargin: '50px'
    });

    // Observe elements that should animate on scroll
    const animatedElements = document.querySelectorAll('.animate-on-scroll, .command-example, .feature-card, .plugin-type-card, .theme-card');
    animatedElements.forEach((el, index) => {
        el.classList.add('animate-ready');
        el.dataset.delay = index * 100; // Stagger animations
        animationObserver.observe(el);
    });
}

/**
 * Setup hover effects and micro-interactions
 */
function setupHoverEffects() {
    // Enhanced button hover effects
    const buttons = document.querySelectorAll('button, .btn, .nav-link, .apply-theme-btn, .install-btn');
    buttons.forEach(button => {
        button.addEventListener('mouseenter', (e) => {
            createRippleEffect(e.target, e);
        });

        button.addEventListener('mouseleave', (e) => {
            e.target.classList.remove('hover-active');
        });
    });

    // Card hover effects
    const cards = document.querySelectorAll('.feature-card, .plugin-type-card, .theme-card, .community-theme');
    cards.forEach(card => {
        card.addEventListener('mouseenter', () => {
            card.style.transform = 'translateY(-4px) scale(1.02)';
            card.style.boxShadow = '0 8px 25px rgba(255, 255, 255, 0.1)';
        });

        card.addEventListener('mouseleave', () => {
            card.style.transform = '';
            card.style.boxShadow = '';
        });
    });

    // Command example hover effects
    const commands = document.querySelectorAll('.copyable-command, .command-example');
    commands.forEach(cmd => {
        cmd.addEventListener('mouseenter', () => {
            cmd.classList.add('hover-glow');
        });

        cmd.addEventListener('mouseleave', () => {
            cmd.classList.remove('hover-glow');
        });
    });
}

/**
 * Create ripple effect on click
 */
function createRippleEffect(element, event) {
    const ripple = document.createElement('span');
    const rect = element.getBoundingClientRect();
    const size = Math.max(rect.width, rect.height);
    const x = event.clientX - rect.left - size / 2;
    const y = event.clientY - rect.top - size / 2;

    ripple.style.width = ripple.style.height = size + 'px';
    ripple.style.left = x + 'px';
    ripple.style.top = y + 'px';
    ripple.classList.add('ripple');

    element.appendChild(ripple);

    setTimeout(() => {
        ripple.remove();
    }, 600);
}

/**
 * Setup loading animations
 */
function setupLoadingAnimations() {
    // Animate loading spinner
    const spinners = document.querySelectorAll('.loading-spinner');
    spinners.forEach(spinner => {
        const frames = ['⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏'];
        let frameIndex = 0;

        const animateSpinner = () => {
            if (spinner.isConnected) {
                spinner.textContent = frames[frameIndex];
                frameIndex = (frameIndex + 1) % frames.length;
                setTimeout(animateSpinner, 100);
            }
        };

        animateSpinner();
    });

    // Progress bar animations
    const progressBars = document.querySelectorAll('.progress-bar');
    progressBars.forEach(bar => {
        const progress = bar.dataset.progress || 0;
        bar.style.width = '0%';

        setTimeout(() => {
            bar.style.width = progress + '%';
        }, 200);
    });
}

/**
 * Setup particle effects for certain themes
 */
function setupParticleEffects() {
    if (typeof particlesJS !== 'undefined') {
        // Matrix-style particles for cyberpunk theme
        particlesJS('particles-js', {
            particles: {
                number: { value: 50 },
                color: { value: '#00ff41' },
                shape: { type: 'circle' },
                opacity: { value: 0.3, random: true },
                size: { value: 2, random: true },
                move: {
                    enable: true,
                    speed: 2,
                    direction: 'bottom',
                    random: false,
                    straight: false,
                    out_mode: 'out',
                    bounce: false
                }
            },
            interactivity: {
                detect_on: 'canvas',
                events: {
                    onhover: { enable: true, mode: 'repulse' },
                    onclick: { enable: true, mode: 'push' }
                }
            },
            retina_detect: true
        });
    }
}

/**
 * Setup typing animations for text elements
 */
function setupTypingAnimations() {
    const typingElements = document.querySelectorAll('.typing, .type-animation');

    typingElements.forEach(element => {
        const text = element.textContent;
        const speed = parseInt(element.dataset.speed) || 50;

        element.textContent = '';
        element.style.borderRight = '2px solid currentColor';

        let i = 0;
        const typeChar = () => {
            if (i < text.length) {
                element.textContent += text.charAt(i);
                i++;
                setTimeout(typeChar, speed);
            } else {
                // Blinking cursor effect
                setInterval(() => {
                    element.style.borderRight = element.style.borderRight === 'none' ?
                        '2px solid currentColor' : 'none';
                }, 500);
            }
        };

        // Start typing after a delay
        setTimeout(typeChar, parseInt(element.dataset.delay) || 500);
    });
}

/**
 * Animate page transitions
 */
function animatePageTransition(direction = 'forward') {
    const content = document.querySelector('.page-content');

    if (direction === 'forward') {
        content.style.animation = 'slideOutLeft 0.3s ease-in-out';
        setTimeout(() => {
            content.style.animation = 'slideInRight 0.3s ease-in-out';
        }, 300);
    } else {
        content.style.animation = 'slideOutRight 0.3s ease-in-out';
        setTimeout(() => {
            content.style.animation = 'slideInLeft 0.3s ease-in-out';
        }, 300);
    }
}

/**
 * Animate search modal
 */
function animateSearchModal(show = true) {
    const modal = document.getElementById('search-modal');
    const container = modal?.querySelector('.search-container');

    if (!modal || !container) return;

    if (show) {
        modal.style.display = 'flex';
        modal.style.opacity = '0';
        container.style.transform = 'translateY(-50px) scale(0.9)';

        requestAnimationFrame(() => {
            modal.style.opacity = '1';
            container.style.transform = 'translateY(0) scale(1)';
        });
    } else {
        modal.style.opacity = '0';
        container.style.transform = 'translateY(-50px) scale(0.9)';

        setTimeout(() => {
            modal.style.display = 'none';
        }, 200);
    }
}

/**
 * Animate notification toasts
 */
function animateNotification(element, type = 'slideIn') {
    element.classList.add('notification-animate');

    switch (type) {
        case 'slideIn':
            element.style.transform = 'translateX(100%)';
            element.style.opacity = '0';

            requestAnimationFrame(() => {
                element.style.transform = 'translateX(0)';
                element.style.opacity = '1';
            });
            break;

        case 'fadeIn':
            element.style.opacity = '0';
            element.style.transform = 'scale(0.8)';

            requestAnimationFrame(() => {
                element.style.opacity = '1';
                element.style.transform = 'scale(1)';
            });
            break;
    }
}

/**
 * Animate element highlight
 */
function animateHighlight(element, color = '#ffffff') {
    const originalBorder = element.style.border;
    const originalBoxShadow = element.style.boxShadow;

    element.style.border = `2px solid ${color}`;
    element.style.boxShadow = `0 0 10px ${color}`;
    element.style.transition = 'all 0.3s ease';

    setTimeout(() => {
        element.style.border = originalBorder;
        element.style.boxShadow = originalBoxShadow;
    }, 1000);
}

/**
 * Staggered animation for lists
 */
function animateList(containerSelector, itemDelay = 100) {
    const container = document.querySelector(containerSelector);
    if (!container) return;

    const items = container.children;
    Array.from(items).forEach((item, index) => {
        item.style.opacity = '0';
        item.style.transform = 'translateY(20px)';

        setTimeout(() => {
            item.style.transition = 'all 0.4s ease';
            item.style.opacity = '1';
            item.style.transform = 'translateY(0)';
        }, index * itemDelay);
    });
}

/**
 * Theme transition animation
 */
function animateThemeChange(newTheme) {
    const body = document.body;

    // Flash effect
    const overlay = document.createElement('div');
    overlay.style.cssText = `
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background: white;
        opacity: 0;
        pointer-events: none;
        z-index: 9999;
        transition: opacity 0.2s ease;
    `;

    body.appendChild(overlay);

    // Flash
    requestAnimationFrame(() => {
        overlay.style.opacity = '0.8';

        setTimeout(() => {
            // Apply theme
            body.className = `theme-${newTheme}`;

            // Fade out flash
            overlay.style.opacity = '0';

            setTimeout(() => {
                overlay.remove();
            }, 200);
        }, 100);
    });
}

/**
 * Add CSS for animations
 */
function addAnimationCSS() {
    const style = document.createElement('style');
    style.textContent = `
        /* Animation Base Classes */
        .animate-ready {
            opacity: 0;
            transform: translateY(20px);
            transition: all 0.6s ease;
        }

        .animate-in {
            opacity: 1;
            transform: translateY(0);
        }

        /* Specific Animations */
        .animate-fadeIn {
            animation: fadeIn 0.6s ease-out;
        }

        .animate-slideUp {
            animation: slideUp 0.6s ease-out;
        }

        .animate-slideLeft {
            animation: slideLeft 0.6s ease-out;
        }

        .animate-zoomIn {
            animation: zoomIn 0.6s ease-out;
        }

        /* Hover Effects */
        .hover-glow {
            box-shadow: 0 0 15px rgba(255, 255, 255, 0.2);
            transform: translateY(-2px);
        }

        .hover-active {
            transform: scale(1.05);
        }

        /* Ripple Effect */
        .ripple {
            position: absolute;
            border-radius: 50%;
            transform: scale(0);
            animation: ripple 0.6s linear;
            background-color: rgba(255, 255, 255, 0.3);
            pointer-events: none;
        }

        /* Keyframes */
        @keyframes fadeIn {
            from { opacity: 0; }
            to { opacity: 1; }
        }

        @keyframes slideUp {
            from {
                opacity: 0;
                transform: translateY(30px);
            }
            to {
                opacity: 1;
                transform: translateY(0);
            }
        }

        @keyframes slideLeft {
            from {
                opacity: 0;
                transform: translateX(30px);
            }
            to {
                opacity: 1;
                transform: translateX(0);
            }
        }

        @keyframes zoomIn {
            from {
                opacity: 0;
                transform: scale(0.8);
            }
            to {
                opacity: 1;
                transform: scale(1);
            }
        }

        @keyframes ripple {
            to {
                transform: scale(2);
                opacity: 0;
            }
        }

        @keyframes slideOutLeft {
            from { transform: translateX(0); opacity: 1; }
            to { transform: translateX(-100%); opacity: 0; }
        }

        @keyframes slideInRight {
            from { transform: translateX(100%); opacity: 0; }
            to { transform: translateX(0); opacity: 1; }
        }

        @keyframes slideOutRight {
            from { transform: translateX(0); opacity: 1; }
            to { transform: translateX(100%); opacity: 0; }
        }

        @keyframes slideInLeft {
            from { transform: translateX(-100%); opacity: 0; }
            to { transform: translateX(0); opacity: 1; }
        }

        /* Notification Animations */
        .notification-animate {
            transition: all 0.3s cubic-bezier(0.68, -0.55, 0.265, 1.55);
        }

        /* Reduced motion support */
        @media (prefers-reduced-motion: reduce) {
            .animate-ready,
            .animate-in,
            .notification-animate,
            .ripple {
                animation: none !important;
                transition: none !important;
            }
        }
    `;

    document.head.appendChild(style);
}

// Initialize animations when DOM is ready
document.addEventListener('DOMContentLoaded', () => {
    addAnimationCSS();
    initAnimations();
});

// Make animation functions available globally
window.animatePageTransition = animatePageTransition;
window.animateSearchModal = animateSearchModal;
window.animateNotification = animateNotification;
window.animateHighlight = animateHighlight;
window.animateList = animateList;
window.animateThemeChange = animateThemeChange;
window.initAnimations = initAnimations;