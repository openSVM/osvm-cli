//! Blueprint Theme System for Consistent UI Styling
//!
//! This module provides a comprehensive theming system that ensures consistent
//! visual appearance across all OSVM interfaces using a professional blueprint design.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Blueprint theme configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BlueprintTheme {
    /// Primary color palette
    pub colors: ColorPalette,
    /// Typography settings
    pub typography: Typography,
    /// Component styling
    pub components: ComponentStyles,
    /// Layout specifications
    pub layout: LayoutSettings,
    /// Animation and transition settings
    pub animations: AnimationSettings,
}

/// Color palette for the blueprint theme
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ColorPalette {
    // Primary blueprint colors
    pub blueprint_blue: String,       // #0078D4 - Primary brand color
    pub blueprint_dark_blue: String,  // #004578 - Darker variant
    pub blueprint_light_blue: String, // #40E0D0 - Accent color

    // Neutral colors
    pub background_primary: String,   // #0A0E0A - Dark background
    pub background_secondary: String, // #1A1E1A - Slightly lighter background
    pub background_tertiary: String,  // #2A2E2A - Cards/panels

    // Text colors
    pub text_primary: String,   // #E8E8E8 - Main text
    pub text_secondary: String, // #B8B8B8 - Secondary text
    pub text_muted: String,     // #888888 - Muted text

    // Status colors
    pub success: String, // #00C851 - Success green
    pub warning: String, // #FFA726 - Warning orange
    pub error: String,   // #FF3547 - Error red
    pub info: String,    // #33B5E5 - Info blue

    // Severity colors for audit findings
    pub critical: String, // #FF1744 - Critical issues
    pub high: String,     // #FF5722 - High severity
    pub medium: String,   // #FF9800 - Medium severity
    pub low: String,      // #4CAF50 - Low severity

    // UI accents
    pub border_primary: String,   // #404040 - Main borders
    pub border_secondary: String, // #303030 - Subtle borders
    pub hover: String,            // #2A4A5A - Hover states
    pub active: String,           // #3A5A6A - Active states
}

/// Typography configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Typography {
    /// Primary font family (monospace for CLI feel)
    pub font_family_primary: String,
    /// Secondary font family
    pub font_family_secondary: String,
    /// Font sizes in rem
    pub font_sizes: FontSizes,
    /// Font weights
    pub font_weights: FontWeights,
    /// Line heights
    pub line_heights: LineHeights,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FontSizes {
    pub xs: String,   // 0.75rem
    pub sm: String,   // 0.875rem
    pub base: String, // 1rem
    pub lg: String,   // 1.125rem
    pub xl: String,   // 1.25rem
    pub xl2: String,  // 1.5rem
    pub xl3: String,  // 1.875rem
    pub xl4: String,  // 2.25rem
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FontWeights {
    pub thin: String,      // 100
    pub light: String,     // 300
    pub normal: String,    // 400
    pub medium: String,    // 500
    pub semibold: String,  // 600
    pub bold: String,      // 700
    pub extrabold: String, // 800
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LineHeights {
    pub tight: String,   // 1.25
    pub normal: String,  // 1.5
    pub relaxed: String, // 1.75
}

/// Component-specific styling
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComponentStyles {
    pub buttons: ButtonStyles,
    pub cards: CardStyles,
    pub navigation: NavigationStyles,
    pub code_blocks: CodeBlockStyles,
    pub tables: TableStyles,
    pub forms: FormStyles,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ButtonStyles {
    pub primary_bg: String,
    pub primary_text: String,
    pub primary_border: String,
    pub secondary_bg: String,
    pub secondary_text: String,
    pub secondary_border: String,
    pub danger_bg: String,
    pub danger_text: String,
    pub border_radius: String,
    pub padding: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CardStyles {
    pub background: String,
    pub border: String,
    pub border_radius: String,
    pub shadow: String,
    pub padding: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NavigationStyles {
    pub background: String,
    pub text: String,
    pub active_bg: String,
    pub active_text: String,
    pub hover_bg: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CodeBlockStyles {
    pub background: String,
    pub text: String,
    pub border: String,
    pub problematic_bg: String, // For problematic code
    pub fixed_bg: String,       // For suggested fixes
    pub line_numbers: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TableStyles {
    pub header_bg: String,
    pub header_text: String,
    pub row_bg: String,
    pub row_alt_bg: String,
    pub border: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FormStyles {
    pub input_bg: String,
    pub input_text: String,
    pub input_border: String,
    pub input_focus_border: String,
    pub label_text: String,
}

/// Layout settings
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LayoutSettings {
    pub max_width: String,
    pub container_padding: String,
    pub section_spacing: String,
    pub element_spacing: String,
    pub border_radius: String,
}

/// Animation settings
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnimationSettings {
    pub transition_duration: String,
    pub easing_function: String,
    pub hover_scale: String,
}

/// Theme manager for generating consistent styles
pub struct BlueprintThemeManager {
    theme: BlueprintTheme,
    css_cache: HashMap<String, String>,
}

impl BlueprintThemeManager {
    pub fn new() -> Self {
        Self {
            theme: BlueprintTheme::default(),
            css_cache: HashMap::new(),
        }
    }

    /// Generate complete CSS for HTML reports
    pub fn generate_html_css(&mut self) -> String {
        if let Some(cached) = self.css_cache.get("html_complete") {
            return cached.clone();
        }

        let css = format!(
            r#"
/* OSVM Blueprint Theme - Complete CSS */
:root {{
    /* Color Variables */
    --bp-blue: {blueprint_blue};
    --bp-dark-blue: {blueprint_dark_blue};
    --bp-light-blue: {blueprint_light_blue};
    --bg-primary: {background_primary};
    --bg-secondary: {background_secondary};
    --bg-tertiary: {background_tertiary};
    --text-primary: {text_primary};
    --text-secondary: {text_secondary};
    --text-muted: {text_muted};
    --success: {success};
    --warning: {warning};
    --error: {error};
    --info: {info};
    --critical: {critical};
    --high: {high};
    --medium: {medium};
    --low: {low};
    --border-primary: {border_primary};
    --border-secondary: {border_secondary};
    --hover: {hover};
    --active: {active};
    
    /* Typography */
    --font-mono: {font_family_primary};
    --font-sans: {font_family_secondary};
}}

/* Base Styles */
* {{
    margin: 0;
    padding: 0;
    box-sizing: border-box;
}}

body {{
    font-family: var(--font-mono);
    background: linear-gradient(135deg, var(--bg-primary) 0%, var(--bg-secondary) 100%);
    color: var(--text-primary);
    line-height: {line_height_normal};
    min-height: 100vh;
}}

/* Container */
.container {{
    max-width: {max_width};
    margin: 0 auto;
    padding: {container_padding};
}}

/* Header Styles */
.site-header {{
    background: linear-gradient(45deg, var(--bp-dark-blue), var(--bp-blue));
    color: white;
    padding: 2rem 0;
    text-align: center;
    position: relative;
    overflow: hidden;
}}

.site-header::before {{
    content: '';
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    background: url('data:image/svg+xml,<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100"><defs><pattern id="grid" width="10" height="10" patternUnits="userSpaceOnUse"><path d="M 10 0 L 0 0 0 10" fill="none" stroke="rgba(255,255,255,0.1)" stroke-width="0.5"/></pattern></defs><rect width="100" height="100" fill="url(%23grid)"/></svg>');
    opacity: 0.3;
}}

.logo {{
    font-size: {font_size_xl4};
    font-weight: {font_weight_bold};
    margin-bottom: 0.5rem;
    text-shadow: 2px 2px 4px rgba(0,0,0,0.3);
    position: relative;
    z-index: 1;
}}

.tagline {{
    font-size: {font_size_lg};
    opacity: 0.9;
    position: relative;
    z-index: 1;
}}

/* Navigation */
.nav-buttons {{
    display: flex;
    justify-content: center;
    gap: 1rem;
    margin-top: 1.5rem;
    flex-wrap: wrap;
    position: relative;
    z-index: 1;
}}

/* Buttons */
.btn {{
    display: inline-block;
    padding: {button_padding};
    border-radius: {button_border_radius};
    text-decoration: none;
    font-weight: {font_weight_semibold};
    font-family: var(--font-mono);
    transition: all {transition_duration} {easing_function};
    border: 2px solid transparent;
    cursor: pointer;
    position: relative;
    overflow: hidden;
}}

.btn::before {{
    content: '';
    position: absolute;
    top: 0;
    left: -100%;
    width: 100%;
    height: 100%;
    background: linear-gradient(90deg, transparent, rgba(255,255,255,0.2), transparent);
    transition: left {transition_duration};
}}

.btn:hover::before {{
    left: 100%;
}}

.btn-primary {{
    background: var(--bp-blue);
    color: white;
    border-color: var(--bp-light-blue);
}}

.btn-primary:hover {{
    background: var(--bp-light-blue);
    transform: translateY(-2px) scale({hover_scale});
    box-shadow: 0 8px 25px rgba(0, 120, 212, 0.3);
}}

.btn-secondary {{
    background: transparent;
    color: white;
    border-color: white;
}}

.btn-secondary:hover {{
    background: white;
    color: var(--bp-blue);
    transform: translateY(-2px);
}}

/* Main Content */
main {{
    background: var(--bg-tertiary);
    margin-top: -2rem;
    border-radius: 1.5rem 1.5rem 0 0;
    position: relative;
    z-index: 1;
    min-height: calc(100vh - 200px);
    box-shadow: 0 -10px 40px rgba(0,0,0,0.3);
}}

.content {{
    padding: 3rem 2rem;
}}

/* Cards and Sections */
.section {{
    background: var(--bg-secondary);
    border-radius: {card_border_radius};
    padding: {card_padding};
    margin-bottom: {section_spacing};
    border: 1px solid var(--border-primary);
    box-shadow: {card_shadow};
    position: relative;
}}

.section::before {{
    content: '';
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
    height: 3px;
    background: linear-gradient(90deg, var(--bp-blue), var(--bp-light-blue));
    border-radius: {card_border_radius} {card_border_radius} 0 0;
}}

/* DeepLogic Section */
.deeplogic-section {{
    background: linear-gradient(135deg, var(--bg-secondary), var(--bg-primary));
    border: 2px solid var(--bp-blue);
    position: relative;
    overflow: hidden;
}}

.deeplogic-section::before {{
    background: linear-gradient(90deg, var(--bp-blue), var(--bp-light-blue), var(--bp-blue));
}}

/* Headings */
h1, h2, h3, h4, h5, h6 {{
    color: var(--text-primary);
    margin-bottom: 1rem;
    font-weight: {font_weight_semibold};
}}

h1 {{ font-size: {font_size_xl3}; }}
h2 {{ font-size: {font_size_xl2}; color: var(--bp-light-blue); }}
h3 {{ font-size: {font_size_xl}; }}
h4 {{ font-size: {font_size_lg}; }}

/* Code Blocks */
.code-block {{
    background: {code_bg};
    border: 1px solid {code_border};
    border-radius: {border_radius};
    padding: 1.5rem;
    margin: 1rem 0;
    overflow-x: auto;
    font-family: var(--font-mono);
    position: relative;
}}

.code-problematic {{
    background: {code_problematic_bg};
    border-left: 4px solid var(--error);
}}

.code-problematic::before {{
    content: '🔴 Problematic Code';
    position: absolute;
    top: -0.5rem;
    left: 1rem;
    background: var(--error);
    color: white;
    padding: 0.25rem 0.75rem;
    border-radius: 1rem;
    font-size: {font_size_xs};
    font-weight: {font_weight_bold};
}}

.code-fixed {{
    background: {code_fixed_bg};
    border-left: 4px solid var(--success);
}}

.code-fixed::before {{
    content: '🟢 Suggested Fix';
    position: absolute;
    top: -0.5rem;
    left: 1rem;
    background: var(--success);
    color: white;
    padding: 0.25rem 0.75rem;
    border-radius: 1rem;
    font-size: {font_size_xs};
    font-weight: {font_weight_bold};
}}

/* Severity Badges */
.severity-critical {{ background: var(--critical); color: white; }}
.severity-high {{ background: var(--high); color: white; }}
.severity-medium {{ background: var(--medium); color: white; }}
.severity-low {{ background: var(--low); color: white; }}

.severity-badge {{
    display: inline-block;
    padding: 0.25rem 0.75rem;
    border-radius: 1rem;
    font-size: {font_size_xs};
    font-weight: {font_weight_bold};
    text-transform: uppercase;
    letter-spacing: 0.5px;
}}

/* Finding Cards */
.finding {{
    background: var(--bg-tertiary);
    border: 1px solid var(--border-primary);
    border-radius: {card_border_radius};
    padding: 1.5rem;
    margin-bottom: 1.5rem;
    position: relative;
    transition: all {transition_duration};
}}

.finding:hover {{
    border-color: var(--bp-blue);
    box-shadow: 0 8px 30px rgba(0, 120, 212, 0.2);
    transform: translateY(-2px);
}}

.finding-header {{
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: 1rem;
    padding-bottom: 0.75rem;
    border-bottom: 1px solid var(--border-secondary);
}}

.finding-id {{
    font-family: var(--font-mono);
    background: var(--bg-secondary);
    padding: 0.25rem 0.5rem;
    border-radius: 0.25rem;
    font-size: {font_size_sm};
    color: var(--text-secondary);
}}

/* Tables */
table {{
    width: 100%;
    border-collapse: collapse;
    background: var(--bg-tertiary);
    border-radius: {border_radius};
    overflow: hidden;
    margin: 1rem 0;
}}

th {{
    background: {table_header_bg};
    color: {table_header_text};
    padding: 1rem;
    text-align: left;
    font-weight: {font_weight_semibold};
    border-bottom: 2px solid var(--bp-blue);
}}

td {{
    padding: 0.75rem 1rem;
    border-bottom: 1px solid var(--border-secondary);
}}

tr:nth-child(even) {{
    background: {table_row_alt_bg};
}}

tr:hover {{
    background: var(--hover);
}}

/* Animations */
@keyframes fadeIn {{
    from {{ opacity: 0; transform: translateY(20px); }}
    to {{ opacity: 1; transform: translateY(0); }}
}}

@keyframes slideIn {{
    from {{ transform: translateX(-100%); }}
    to {{ transform: translateX(0); }}
}}

.fade-in {{
    animation: fadeIn 0.6s ease-out;
}}

.slide-in {{
    animation: slideIn 0.4s ease-out;
}}

/* Responsive Design */
@media (max-width: 768px) {{
    .container {{
        padding: 1rem;
    }}
    
    .nav-buttons {{
        flex-direction: column;
        align-items: center;
    }}
    
    .finding-header {{
        flex-direction: column;
        align-items: flex-start;
        gap: 0.5rem;
    }}
    
    .code-block {{
        padding: 1rem;
        font-size: {font_size_sm};
    }}
}}

/* Print Styles */
@media print {{
    body {{
        background: white;
        color: black;
    }}
    
    .site-header {{
        background: var(--bp-blue);
        color: white;
    }}
    
    main {{
        background: white;
        box-shadow: none;
    }}
}}
"#,
            blueprint_blue = self.theme.colors.blueprint_blue,
            blueprint_dark_blue = self.theme.colors.blueprint_dark_blue,
            blueprint_light_blue = self.theme.colors.blueprint_light_blue,
            background_primary = self.theme.colors.background_primary,
            background_secondary = self.theme.colors.background_secondary,
            background_tertiary = self.theme.colors.background_tertiary,
            text_primary = self.theme.colors.text_primary,
            text_secondary = self.theme.colors.text_secondary,
            text_muted = self.theme.colors.text_muted,
            success = self.theme.colors.success,
            warning = self.theme.colors.warning,
            error = self.theme.colors.error,
            info = self.theme.colors.info,
            critical = self.theme.colors.critical,
            high = self.theme.colors.high,
            medium = self.theme.colors.medium,
            low = self.theme.colors.low,
            border_primary = self.theme.colors.border_primary,
            border_secondary = self.theme.colors.border_secondary,
            hover = self.theme.colors.hover,
            active = self.theme.colors.active,
            font_family_primary = self.theme.typography.font_family_primary,
            font_family_secondary = self.theme.typography.font_family_secondary,
            font_size_xs = self.theme.typography.font_sizes.xs,
            font_size_sm = self.theme.typography.font_sizes.sm,
            font_size_base = self.theme.typography.font_sizes.base,
            font_size_lg = self.theme.typography.font_sizes.lg,
            font_size_xl = self.theme.typography.font_sizes.xl,
            font_size_xl2 = self.theme.typography.font_sizes.xl2,
            font_size_xl3 = self.theme.typography.font_sizes.xl3,
            font_size_xl4 = self.theme.typography.font_sizes.xl4,
            font_weight_normal = self.theme.typography.font_weights.normal,
            font_weight_medium = self.theme.typography.font_weights.medium,
            font_weight_semibold = self.theme.typography.font_weights.semibold,
            font_weight_bold = self.theme.typography.font_weights.bold,
            line_height_normal = self.theme.typography.line_heights.normal,
            max_width = self.theme.layout.max_width,
            container_padding = self.theme.layout.container_padding,
            section_spacing = self.theme.layout.section_spacing,
            border_radius = self.theme.layout.border_radius,
            card_border_radius = self.theme.components.cards.border_radius,
            card_padding = self.theme.components.cards.padding,
            card_shadow = self.theme.components.cards.shadow,
            button_padding = self.theme.components.buttons.padding,
            button_border_radius = self.theme.components.buttons.border_radius,
            code_bg = self.theme.components.code_blocks.background,
            code_border = self.theme.components.code_blocks.border,
            code_problematic_bg = self.theme.components.code_blocks.problematic_bg,
            code_fixed_bg = self.theme.components.code_blocks.fixed_bg,
            table_header_bg = self.theme.components.tables.header_bg,
            table_header_text = self.theme.components.tables.header_text,
            table_row_alt_bg = self.theme.components.tables.row_alt_bg,
            transition_duration = self.theme.animations.transition_duration,
            easing_function = self.theme.animations.easing_function,
            hover_scale = self.theme.animations.hover_scale,
        );

        self.css_cache
            .insert("html_complete".to_string(), css.clone());
        css
    }

    /// Generate CLI color scheme for terminal output
    pub fn get_cli_colors(&self) -> CliColorScheme {
        CliColorScheme {
            primary: "\x1b[38;2;0;120;212m".to_string(), // Blueprint blue
            secondary: "\x1b[38;2;64;224;208m".to_string(), // Blueprint light blue
            success: "\x1b[38;2;0;200;81m".to_string(),  // Success green
            warning: "\x1b[38;2;255;167;38m".to_string(), // Warning orange
            error: "\x1b[38;2;255;53;71m".to_string(),   // Error red
            muted: "\x1b[38;2;136;136;136m".to_string(), // Muted text
            reset: "\x1b[0m".to_string(),                // Reset
            bold: "\x1b[1m".to_string(),                 // Bold
            dim: "\x1b[2m".to_string(),                  // Dim
        }
    }

    /// Get theme configuration
    pub fn get_theme(&self) -> &BlueprintTheme {
        &self.theme
    }

    /// Update theme colors
    pub fn update_colors(&mut self, colors: ColorPalette) {
        self.theme.colors = colors;
        self.css_cache.clear(); // Invalidate cache
    }
}

/// CLI color scheme for terminal output
#[derive(Debug, Clone)]
pub struct CliColorScheme {
    pub primary: String,
    pub secondary: String,
    pub success: String,
    pub warning: String,
    pub error: String,
    pub muted: String,
    pub reset: String,
    pub bold: String,
    pub dim: String,
}

impl Default for BlueprintTheme {
    fn default() -> Self {
        Self {
            colors: ColorPalette {
                blueprint_blue: "#0078D4".to_string(),
                blueprint_dark_blue: "#004578".to_string(),
                blueprint_light_blue: "#40E0D0".to_string(),
                background_primary: "#0A0E0A".to_string(),
                background_secondary: "#1A1E1A".to_string(),
                background_tertiary: "#2A2E2A".to_string(),
                text_primary: "#E8E8E8".to_string(),
                text_secondary: "#B8B8B8".to_string(),
                text_muted: "#888888".to_string(),
                success: "#00C851".to_string(),
                warning: "#FFA726".to_string(),
                error: "#FF3547".to_string(),
                info: "#33B5E5".to_string(),
                critical: "#FF1744".to_string(),
                high: "#FF5722".to_string(),
                medium: "#FF9800".to_string(),
                low: "#4CAF50".to_string(),
                border_primary: "#404040".to_string(),
                border_secondary: "#303030".to_string(),
                hover: "#2A4A5A".to_string(),
                active: "#3A5A6A".to_string(),
            },
            typography: Typography {
                font_family_primary: "'Monaco', 'Menlo', 'Ubuntu Mono', 'Consolas', 'source-code-pro', monospace".to_string(),
                font_family_secondary: "-apple-system, BlinkMacSystemFont, 'Segoe UI', 'Roboto', 'Oxygen', 'Ubuntu', 'Cantarell', 'Open Sans', 'Helvetica Neue', sans-serif".to_string(),
                font_sizes: FontSizes {
                    xs: "0.75rem".to_string(),
                    sm: "0.875rem".to_string(),
                    base: "1rem".to_string(),
                    lg: "1.125rem".to_string(),
                    xl: "1.25rem".to_string(),
                    xl2: "1.5rem".to_string(),
                    xl3: "1.875rem".to_string(),
                    xl4: "2.25rem".to_string(),
                },
                font_weights: FontWeights {
                    thin: "100".to_string(),
                    light: "300".to_string(),
                    normal: "400".to_string(),
                    medium: "500".to_string(),
                    semibold: "600".to_string(),
                    bold: "700".to_string(),
                    extrabold: "800".to_string(),
                },
                line_heights: LineHeights {
                    tight: "1.25".to_string(),
                    normal: "1.5".to_string(),
                    relaxed: "1.75".to_string(),
                },
            },
            components: ComponentStyles {
                buttons: ButtonStyles {
                    primary_bg: "#0078D4".to_string(),
                    primary_text: "#FFFFFF".to_string(),
                    primary_border: "#40E0D0".to_string(),
                    secondary_bg: "transparent".to_string(),
                    secondary_text: "#FFFFFF".to_string(),
                    secondary_border: "#FFFFFF".to_string(),
                    danger_bg: "#FF3547".to_string(),
                    danger_text: "#FFFFFF".to_string(),
                    border_radius: "0.5rem".to_string(),
                    padding: "0.75rem 1.5rem".to_string(),
                },
                cards: CardStyles {
                    background: "#2A2E2A".to_string(),
                    border: "#404040".to_string(),
                    border_radius: "0.75rem".to_string(),
                    shadow: "0 4px 20px rgba(0, 0, 0, 0.3)".to_string(),
                    padding: "1.5rem".to_string(),
                },
                navigation: NavigationStyles {
                    background: "#1A1E1A".to_string(),
                    text: "#E8E8E8".to_string(),
                    active_bg: "#0078D4".to_string(),
                    active_text: "#FFFFFF".to_string(),
                    hover_bg: "#2A4A5A".to_string(),
                },
                code_blocks: CodeBlockStyles {
                    background: "#1A1E1A".to_string(),
                    text: "#E8E8E8".to_string(),
                    border: "#404040".to_string(),
                    problematic_bg: "rgba(255, 53, 71, 0.1)".to_string(),
                    fixed_bg: "rgba(0, 200, 81, 0.1)".to_string(),
                    line_numbers: "#888888".to_string(),
                },
                tables: TableStyles {
                    header_bg: "#0078D4".to_string(),
                    header_text: "#FFFFFF".to_string(),
                    row_bg: "#2A2E2A".to_string(),
                    row_alt_bg: "#1A1E1A".to_string(),
                    border: "#404040".to_string(),
                },
                forms: FormStyles {
                    input_bg: "#1A1E1A".to_string(),
                    input_text: "#E8E8E8".to_string(),
                    input_border: "#404040".to_string(),
                    input_focus_border: "#0078D4".to_string(),
                    label_text: "#B8B8B8".to_string(),
                },
            },
            layout: LayoutSettings {
                max_width: "1200px".to_string(),
                container_padding: "0 2rem".to_string(),
                section_spacing: "2rem".to_string(),
                element_spacing: "1rem".to_string(),
                border_radius: "0.5rem".to_string(),
            },
            animations: AnimationSettings {
                transition_duration: "0.3s".to_string(),
                easing_function: "cubic-bezier(0.4, 0, 0.2, 1)".to_string(),
                hover_scale: "1.02".to_string(),
            },
        }
    }
}

impl Default for BlueprintThemeManager {
    fn default() -> Self {
        Self::new()
    }
}
