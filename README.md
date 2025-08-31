# GitHub Stars Browser

A modern web application for browsing GitHub starred repositories with filtering, search, and sorting capabilities.

## Features

- **Google-inspired Home Page**: Clean, minimalist design with centered search input
- **GitHub API Integration**: Fetches starred repositories for any GitHub user
- **Topic Filtering**: Multi-select topic filtering with sidebar navigation
- **Search & Sort**: Search repositories and sort by stars, date, or name
- **Responsive Design**: Works seamlessly on mobile and desktop
- **User Information**: Displays user avatar and profile information
- **Repository Statistics**: Shows total and filtered repository counts

## Tech Stack

- [Elm](https://elm-lang.org/) - Pure functional language for robust frontend development
- [Tailwind CSS](https://tailwindcss.com/) v4 - Utility-first CSS framework
- [DaisyUI](https://daisyui.com/) v5 - Component library for Tailwind CSS
- [Vite](https://vitejs.dev/) - Fast build tool and development server

## Getting Started

```sh
# Clone the repository
git clone <repository-url>
cd stargazer

# Install dependencies
bun install

# Start development server
bun dev

# Open http://localhost:5173 in your browser
```

## Available Scripts

- `bun dev` - Start development server with hot reload
- `bun build` - Build for production
- `bun test` - Run Elm tests
- `bun lint` - Run Elm linter
- `bun compile` - Check Elm compilation
- `bun interop` - Generate TypeScript types from Elm

## Usage

1. **Home Page**: Enter a GitHub username in the search field
2. **Browse Repositories**: View all starred repositories in a clean card layout
3. **Filter by Topics**: Use the sidebar to filter repositories by topics
4. **Search**: Use the search bar to find specific repositories
5. **Sort**: Sort repositories by stars, update date, or name
6. **View Details**: Click on repository names to visit GitHub

## API Usage

The application uses the GitHub REST API:
- `GET /users/{username}` - Fetch user information
- `GET /users/{username}/starred` - Fetch starred repositories

Note: GitHub API has rate limits for unauthenticated requests (60 per hour). For higher limits, consider adding authentication.

## Project Structure

```
src/
├── Main.elm          # Main application logic
├── Routes.elm        # URL routing
├── InteropDefinitions.elm  # TypeScript interop types
├── InteropPorts.elm  # Port definitions
└── Main.elm.d.ts     # Generated TypeScript types
```
