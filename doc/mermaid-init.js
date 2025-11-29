// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

(() => {
    const darkThemes = ['ayu', 'navy', 'coal'];
    const lightThemes = ['light', 'rust'];

    const classList = document.getElementsByTagName('html')[0].classList;

    let lastThemeWasLight = true;
    for (const cssClass of classList) {
        if (darkThemes.includes(cssClass)) {
            lastThemeWasLight = false;
            break;
        }
    }

    const theme = lastThemeWasLight ? 'default' : 'dark';
    
    // Custom theme configuration for better text contrast
    const themeVariables = lastThemeWasLight ? {
        // Light theme: dark text on light backgrounds
        primaryTextColor: '#000000',
        primaryBorderColor: '#333333',
        lineColor: '#333333',
        secondaryTextColor: '#000000',
        tertiaryTextColor: '#000000',
        // Node colors - lighter backgrounds for better contrast with dark text
        primaryColor: '#f5f5f5',
        secondaryColor: '#e8e8e8',
        tertiaryColor: '#dcdcdc',
        // Text colors for nodes - ensure high contrast
        textColor: '#000000',
        // Background colors
        background: '#ffffff',
        mainBkg: '#f5f5f5',
        secondBkg: '#e8e8e8',
        fontFamily: 'inherit',
        fontSize: '16px',
    } : {
        // Dark theme: light text on dark backgrounds
        primaryTextColor: '#ffffff',
        primaryBorderColor: '#cccccc',
        lineColor: '#cccccc',
        secondaryTextColor: '#ffffff',
        tertiaryTextColor: '#ffffff',
        // Node colors - darker backgrounds for better contrast with light text
        primaryColor: '#1e1e1e',
        secondaryColor: '#2a2a2a',
        tertiaryColor: '#363636',
        // Text colors for nodes - ensure high contrast
        textColor: '#ffffff',
        // Background colors
        background: '#0d1117',
        mainBkg: '#1e1e1e',
        secondBkg: '#2a2a2a',
        fontFamily: 'inherit',
        fontSize: '16px',
    };
    
    mermaid.initialize({ 
        startOnLoad: true, 
        theme: theme,
        themeVariables: themeVariables
    });
    
    // Inject CSS for better text contrast in Mermaid diagrams
    const style = document.createElement('style');
    style.id = 'mermaid-contrast-styles';
    style.textContent = lastThemeWasLight ? `
        /* Light theme: dark text on light backgrounds */
        .mermaid svg text,
        .mermaid svg text tspan,
        .mermaid svg .nodeLabel,
        .mermaid svg .edgeLabel,
        .mermaid svg .cluster-label text,
        .mermaid svg .label text,
        .mermaid svg .node-label,
        .mermaid svg .edge-label,
        .mermaid svg .titleText,
        .mermaid svg .noteText {
            fill: #000000 !important;
            color: #000000 !important;
        }
        .mermaid svg .node rect,
        .mermaid svg .node circle,
        .mermaid svg .node ellipse,
        .mermaid svg .node polygon,
        .mermaid svg .node path {
            fill: #f5f5f5 !important;
            stroke: #333333 !important;
        }
        .mermaid svg .flowchart-link,
        .mermaid svg .edgePath .path {
            stroke: #333333 !important;
        }
    ` : `
        /* Dark theme: default to light text, but will be overridden by JS for light backgrounds */
        .mermaid svg text,
        .mermaid svg text tspan,
        .mermaid svg .nodeLabel,
        .mermaid svg .edgeLabel,
        .mermaid svg .cluster-label text,
        .mermaid svg .label text,
        .mermaid svg .node-label,
        .mermaid svg .edge-label,
        .mermaid svg .titleText,
        .mermaid svg .noteText {
            fill: #ffffff;
            color: #ffffff;
        }
        /* Only override node backgrounds if they don't have custom fills */
        .mermaid svg .node rect:not([fill]),
        .mermaid svg .node circle:not([fill]),
        .mermaid svg .node ellipse:not([fill]),
        .mermaid svg .node polygon:not([fill]),
        .mermaid svg .node path:not([fill]) {
            fill: #1e1e1e !important;
            stroke: #cccccc !important;
        }
        .mermaid svg .flowchart-link,
        .mermaid svg .edgePath .path {
            stroke: #cccccc !important;
        }
        .mermaid svg .arrowheadPath {
            fill: #cccccc !important;
        }
    `;
    document.head.appendChild(style);
    
    // Function to calculate luminance of a color (for contrast detection)
    function getLuminance(color) {
        // Convert hex to RGB
        const hex = color.replace('#', '');
        const r = parseInt(hex.substr(0, 2), 16) / 255;
        const g = parseInt(hex.substr(2, 2), 16) / 255;
        const b = parseInt(hex.substr(4, 2), 16) / 255;
        
        // Apply gamma correction
        const [r2, g2, b2] = [r, g, b].map(val => {
            return val <= 0.03928 ? val / 12.92 : Math.pow((val + 0.055) / 1.055, 2.4);
        });
        
        // Calculate relative luminance
        return 0.2126 * r2 + 0.7152 * g2 + 0.0722 * b2;
    }
    
    // Function to get background color of a node
    function getNodeBackgroundColor(nodeElement) {
        // Check for fill attribute
        let fill = nodeElement.getAttribute('fill');
        if (fill && fill !== 'none' && fill !== 'transparent') {
            return fill;
        }
        
        // Check computed style
        const computed = window.getComputedStyle(nodeElement);
        fill = computed.fill;
        if (fill && fill !== 'none' && fill !== 'transparent' && fill !== 'rgb(0, 0, 0)') {
            return fill;
        }
        
        // Check parent for fill
        const parent = nodeElement.parentElement;
        if (parent) {
            fill = parent.getAttribute('fill');
            if (fill && fill !== 'none' && fill !== 'transparent') {
                return fill;
            }
        }
        
        return null;
    }
    
    // Function to convert any color format to hex
    function colorToHex(color) {
        if (!color) return null;
        
        // Already hex
        if (color.startsWith('#')) {
            return color.length === 4 ? 
                '#' + color[1] + color[1] + color[2] + color[2] + color[3] + color[3] : 
                color;
        }
        
        // RGB/RGBA
        if (color.startsWith('rgb')) {
            const rgb = color.match(/\d+/g);
            if (rgb && rgb.length >= 3) {
                return '#' + rgb.slice(0, 3).map(x => {
                    const hex = parseInt(x).toString(16);
                    return hex.length === 1 ? '0' + hex : hex;
                }).join('');
            }
        }
        
        // Named colors (basic set)
        const namedColors = {
            'white': '#ffffff', 'black': '#000000', 'red': '#ff0000',
            'green': '#008000', 'blue': '#0000ff', 'yellow': '#ffff00',
            'cyan': '#00ffff', 'magenta': '#ff00ff', 'gray': '#808080'
        };
        if (namedColors[color.toLowerCase()]) {
            return namedColors[color.toLowerCase()];
        }
        
        return null;
    }
    
    // Function to set text color based on background
    function setTextContrast(svg) {
        // In Mermaid, nodes are typically in <g> groups containing both shape and text
        // Find all groups that contain shapes (these are the nodes)
        const allGroups = svg.querySelectorAll('g');
        const processedTexts = new Set();
        
        allGroups.forEach(group => {
            // Find the shape element (rect, circle, ellipse, polygon, path)
            const shape = group.querySelector('rect, circle, ellipse, polygon, path');
            if (!shape) return;
            
            // Get the fill color of the shape
            let fillColor = shape.getAttribute('fill');
            if (!fillColor || fillColor === 'none' || fillColor === 'transparent') {
                // Try computed style
                const computed = window.getComputedStyle(shape);
                fillColor = computed.fill;
            }
            
            // Convert to hex
            const hexColor = colorToHex(fillColor);
            if (!hexColor) {
                // If no fill color, use theme default
                const textColor = lastThemeWasLight ? '#000000' : '#ffffff';
                group.querySelectorAll('text').forEach(text => {
                    if (!processedTexts.has(text)) {
                        text.setAttribute('fill', textColor);
                        text.querySelectorAll('tspan').forEach(tspan => {
                            tspan.setAttribute('fill', textColor);
                        });
                        processedTexts.add(text);
                    }
                });
                return;
            }
            
            // Calculate luminance and determine text color
            const luminance = getLuminance(hexColor);
            const textColor = luminance > 0.5 ? '#000000' : '#ffffff';
            
            // Find all text elements in this group and set their color
            const texts = group.querySelectorAll('text');
            texts.forEach(text => {
                if (!processedTexts.has(text)) {
                    text.setAttribute('fill', textColor);
                    // Also set on tspan children
                    text.querySelectorAll('tspan').forEach(tspan => {
                        tspan.setAttribute('fill', textColor);
                    });
                    processedTexts.add(text);
                }
            });
        });
        
        // Also handle any remaining text elements (edge labels, etc.)
        const allTexts = svg.querySelectorAll('text');
        allTexts.forEach(text => {
            if (processedTexts.has(text)) return;
            
            // For edge labels and other text, use theme-based default
            text.setAttribute('fill', lastThemeWasLight ? '#000000' : '#ffffff');
            text.querySelectorAll('tspan').forEach(tspan => {
                tspan.setAttribute('fill', lastThemeWasLight ? '#000000' : '#ffffff');
            });
        });
    }
    
    // Function to process all Mermaid diagrams
    function processMermaidDiagrams() {
        const mermaidSvgs = document.querySelectorAll('.mermaid svg');
        if (mermaidSvgs.length === 0) return;
        
        mermaidSvgs.forEach(svg => {
            // Only process if SVG has content
            if (svg.querySelector('g')) {
                setTextContrast(svg);
            }
        });
    }
    
    // Wait for Mermaid to finish rendering
    function waitForMermaid() {
        const maxAttempts = 20;
        let attempts = 0;
        
        const checkMermaid = () => {
            attempts++;
            const mermaidSvgs = document.querySelectorAll('.mermaid svg');
            const hasContent = Array.from(mermaidSvgs).some(svg => svg.querySelector('g.node, g[class*="node"]'));
            
            if (hasContent || attempts >= maxAttempts) {
                processMermaidDiagrams();
                if (attempts < maxAttempts) {
                    // Process again after a short delay to catch any late-rendering elements
                    setTimeout(processMermaidDiagrams, 200);
                }
            } else {
                setTimeout(checkMermaid, 100);
            }
        };
        
        checkMermaid();
    }
    
    // Process diagrams after initial load
    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', () => {
            setTimeout(waitForMermaid, 100);
        });
    } else {
        setTimeout(waitForMermaid, 100);
    }
    
    // Reapply styles after Mermaid renders (in case of dynamic rendering)
    const observer = new MutationObserver((mutations) => {
        let shouldProcess = false;
        mutations.forEach(mutation => {
            if (mutation.addedNodes.length > 0) {
                Array.from(mutation.addedNodes).forEach(node => {
                    if (node.nodeType === 1) { // Element node
                        if (node.classList?.contains('mermaid') || 
                            node.querySelector?.('.mermaid') ||
                            (node.tagName === 'svg' && node.closest('.mermaid'))) {
                            shouldProcess = true;
                        }
                    }
                });
            }
        });
        
        if (shouldProcess) {
            setTimeout(processMermaidDiagrams, 300);
        }
    });
    
    // Observe for new Mermaid diagrams
    observer.observe(document.body, {
        childList: true,
        subtree: true
    });

    // Simplest way to make mermaid re-render the diagrams in the new theme is via refreshing the page

    for (const darkTheme of darkThemes) {
        document.getElementById(darkTheme).addEventListener('click', () => {
            if (lastThemeWasLight) {
                window.location.reload();
            }
        });
    }

    for (const lightTheme of lightThemes) {
        document.getElementById(lightTheme).addEventListener('click', () => {
            if (!lastThemeWasLight) {
                window.location.reload();
            }
        });
    }
})();
