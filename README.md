# Fjäril - fjaril 𓂃 ࣪˖ ִֶָ𐀔

**Fjäril** (Swedish for _butterfly_) is a cute little open-source programming language designed to make coding a little more unique! Inspired by the lightness and uniqueness of butterflies, we hope Fjäril makes you feel like you can express yourself more with code. Whether you're into pastel colours or cutesy syntax, Fjäril helps you code in a way that feels uniquely like _you_.

## Table of Contents

- [Installation](#installation)
- [Getting Started](#getting-started)
- [Syntax Overview](#syntax-overview)
- [Data Types](#data-types)
- [Control Structures](#control-structures)
- [Functions](#functions)
- [Built-in Functions](#built-in-functions)
- [Standard Library](#standard-library)
- [Examples](#examples)
- [Contributing](#contributing)
- [License](#license)

## Installation

## Getting Started

## Syntax Overview

## Data Types

## Control Structures

## Built-in Functions

## Standard Library

## Examples

## Contributing

## License

⠀⠀⠀⠀⣀⣀⣀⣀⣀⣀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢴⣤⣄⠀⠀⠀⠀⠀⢀⣤⣶⠄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣀⣀⣀⣀⣀⡀⠀⠀⠀⠀
⢴⣾⣿⣿⣿⣏⣉⣉⣉⣛⣛⣻⣿⣶⣦⣤⣀⠀⠀⠀⠀⠀⠀⠙⢷⠀⠀⠀⢠⡟⠀⠀⠀⠀⠀⠀⢀⣠⣤⣶⣾⣿⣛⣛⣋⣉⣉⣉⣿⣿⣿⣿⣶⠄
⠀⠙⢿⣿⣿⣿⡁⠀⠀⣉⡿⠋⠉⠉⠙⠛⠻⢿⣦⣄⠀⠀⠀⠀⠘⣇⠀⢀⡟⠀⠀⠀⠀⢀⣤⣾⠿⠛⠋⠉⠉⠉⠻⣏⡉⠀⠈⣹⣿⣿⣿⠟⠁⠀
⠀⠀⠀⠙⣿⣿⣟⠉⠉⣙⡇⠀⠀⠀⠀⠀⠀⠀⠈⠛⢿⣦⡀⠀⠀⢹⠀⣸⠃⠀⠀⣠⣾⠟⠋⠀⠀⠀⠀⠀⠀⠀⢀⣿⡉⠉⠹⣿⣿⡟⠉⠀⠀⠀
⠀⠀⠀⠀⠈⢿⣿⣟⠉⢉⣿⣦⣄⣀⣀⠀⠀⠀⠀⠀⠀⠙⢿⣦⡀⠸⡇⡿⠀⣠⣾⠟⠁⠀⠀⠀⠀⠀⢀⣀⣀⣤⣾⣍⠉⠙⣿⣿⡯⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠸⣿⣿⣿⠉⠀⠀⣀⣨⡟⠛⠛⠒⠒⠤⢄⣀⠀⠙⢿⣆⣷⣧⣾⠟⠁⠀⣀⠤⠄⠒⠚⠛⠛⣯⣀⡀⠀⠉⢹⣿⣿⡿⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠸⣿⣿⣿⡟⠉⣀⡤⢷⡦⠄⠀⠀⠀⠀⠈⠙⠢⣌⣻⣿⣿⢋⠤⠚⠉⠀⠀⠀⠀⠀⠠⣶⠷⣄⡈⠙⣿⣿⣿⡿⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠸⠿⣿⣿⣿⣁⡤⠾⣷⣤⣴⠶⢶⣒⡻⠿⠿⢛⣿⣿⣿⣟⠛⠽⠿⢓⣲⠶⢶⣦⣴⡟⠦⣄⣹⣿⣿⡿⠟⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠛⠛⣿⣿⣿⢛⣉⠥⠒⠉⠁⠀⣀⠔⢈⠔⣽⣿⣿⡝⢌⠒⢄⡀⠀⠉⠑⠢⢍⣙⢻⣿⣿⡟⠛⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣠⣾⣿⠋⠀⠀⠀⢀⡤⠊⠀⡴⠋⣼⡟⣿⡟⢿⡄⠳⡄⠈⠢⣀⠀⠀⠀⠈⢻⣷⣤⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⣿⣯⢽⣀⠀⣀⣴⡏⠀⢀⡞⠁⣸⡿⠀⠉⠀⠘⣿⡀⠙⣆⠀⠘⣷⣄⡀⢀⡸⢯⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠚⣿⣿⡶⠋⡿⠋⣽⠷⣶⡟⠀⢰⣿⠃⠀⠀⠀⠀⢹⣧⠀⠘⣷⡖⢿⡍⠻⡏⠳⣶⣿⡟⠂⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⣿⣿⣷⡞⠀⢠⡇⢰⡟⠀⠀⣾⡏⠀⠀⠀⠀⠀⠈⣿⡆⠀⠘⣧⠀⣷⠀⠹⣶⣿⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⣿⣿⣶⡟⠙⣿⡀⠀⣰⡿⠀⠀⠀⠀⠀⠀⠀⠸⣷⡀⠀⣹⣟⠙⣷⣾⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠉⠙⢿⣿⣿⣿⡿⠿⠿⠛⠁⠀⠀⠀⠀⠀⠀⠀⠀⠙⠿⠿⠿⣿⣿⣿⣿⠟⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⣿⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢹⣧⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣰⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⣷⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢿⣿⠇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢿⣿⠇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
