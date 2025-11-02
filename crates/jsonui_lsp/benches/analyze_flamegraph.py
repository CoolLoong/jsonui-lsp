#!/usr/bin/env python3
"""
Flamegraph Analysis Tool

Analyzes Rust flamegraph SVG files to identify performance hotspots.
Usage: python analyze_flamegraph.py <path_to_flamegraph.svg>
"""
import re
import sys
from pathlib import Path


def parse_flamegraph(svg_path):
    """Parse flamegraph SVG and extract function samples."""
    with open(svg_path, 'r', encoding='utf-8') as f:
        content = f.read()

    # Extract all title tags
    titles = re.findall(r'<title>([^<]+)</title>', content)

    # Parse function names and sample counts
    results = []
    for title in titles:
        match = re.match(r'(.+?)\s+\((\d+(?:,\d+)*)\s+samples?,\s+([\d.]+)%\)', title)
        if match:
            func = match.group(1)
            samples = int(match.group(2).replace(',', ''))
            percent = float(match.group(3))
            results.append((func, samples, percent))

    return results


def filter_functions(results, include_pattern=None, exclude_pattern=None):
    """Filter functions by include/exclude patterns."""
    filtered = []
    for func, samples, percent in results:
        if include_pattern and include_pattern not in func:
            continue
        if exclude_pattern and exclude_pattern in func:
            continue
        filtered.append((func, samples, percent))
    return filtered


def group_by_category(results):
    """Group results by function categories."""
    categories = {
        'DocumentParser::new & HashMap creation': [],
        'DocumentParser::hashmap': [],
        'parse_key_value_pair': [],
        'parse_value': [],
        'DocumentParser::string': [],
        'DocumentParser::text': [],
        'Completer operations': [],
        'Other operations': []
    }

    for func, samples, percent in results:
        if 'DocumentParser::new' in func:
            categories['DocumentParser::new & HashMap creation'].append((func, samples, percent))
        elif 'DocumentParser::hashmap' in func:
            categories['DocumentParser::hashmap'].append((func, samples, percent))
        elif 'parse_key_value_pair' in func:
            categories['parse_key_value_pair'].append((func, samples, percent))
        elif 'parse_value' in func:
            categories['parse_value'].append((func, samples, percent))
        elif 'DocumentParser::string' in func:
            categories['DocumentParser::string'].append((func, samples, percent))
        elif 'DocumentParser::text' in func:
            categories['DocumentParser::text'].append((func, samples, percent))
        elif 'completer' in func:
            categories['Completer operations'].append((func, samples, percent))
        else:
            categories['Other operations'].append((func, samples, percent))

    return categories


def print_category(name, items, max_items=5):
    """Print a category of functions."""
    if not items:
        return

    total = sum(s for _, s, _ in items)
    print(f'\n{name}: {total} samples total')
    print('-' * 80)

    for func, samples, percent in items[:max_items]:
        # Shorten function name for readability
        short_func = func.split('::')[-1] if '::' in func else func
        if len(short_func) > 70:
            short_func = short_func[:67] + '...'
        print(f'{samples:>6} ({percent:>6.2f}%) {short_func}')


def analyze_runtime_hotspots(svg_path, exclude_startup=True):
    """Analyze runtime performance hotspots."""
    print(f'Analyzing flamegraph: {svg_path}')
    print('=' * 80)

    results = parse_flamegraph(svg_path)

    # Filter for jsonui_lsp functions
    jsonui_funcs = filter_functions(results, include_pattern='jsonui_lsp')

    if exclude_startup:
        jsonui_funcs = filter_functions(
            jsonui_funcs
        )
        print('Runtime hotspots (excluding load_vanilla_controls_table):')
    else:
        print('All jsonui_lsp hotspots:')

    print('=' * 80)

    # Group by category
    categories = group_by_category(jsonui_funcs)

    # Sort categories by total samples
    sorted_categories = sorted(
        categories.items(),
        key=lambda x: sum(s for _, s, _ in x[1]),
        reverse=True
    )

    for name, items in sorted_categories:
        if items:
            print_category(name, items)

    # Print top 10 overall
    print('\n' + '=' * 80)
    print('Top 10 Runtime Functions:')
    print('=' * 80)
    jsonui_funcs.sort(key=lambda x: x[1], reverse=True)
    for func, samples, percent in jsonui_funcs[:10]:
        print(f'{samples:>6} ({percent:>6.2f}%) {func}')


def main():
    if len(sys.argv) < 2:
        print("Usage: python analyze_flamegraph.py <path_to_flamegraph.svg>")
        sys.exit(1)

    svg_path = Path(sys.argv[1])
    if not svg_path.exists():
        print(f"Error: File not found: {svg_path}")
        sys.exit(1)

    analyze_runtime_hotspots(svg_path, exclude_startup=True)


if __name__ == '__main__':
    main()