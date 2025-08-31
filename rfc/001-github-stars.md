You are a senior full-stack developer tasked with building a GitHub starred repositories browser web application. Create a complete, production-ready application with the following specifications:

Core Functionality:

Home Page: Design a Google-inspired landing page with a centered input field for GitHub username entry
Data Fetching: Implement GitHub API integration to fetch all starred repositories for a given user, ensuring you handle pagination by following all pagination links in response headers. Stars API endpoint is https://api.github.com/users/<username>/starred
Repository Display: Show fetched repositories in a clean, organized layout
Topic Filtering: Extract unique topics from all repositories and display them in a sidebar for filtering
Multi-select Filtering: Allow users to select multiple topics simultaneously, showing only repositories that contain ALL selected topics
Technical Requirements:

Use daisy UI components throughout the application
Implement the sidebar component for topic navigation
Handle API errors gracefully (user not found, rate limiting, network errors)
Show loading states during data fetching
Ensure responsive design for mobile and desktop
Use TypeScript for type safety
Implement proper state management for filters and repository data
UI/UX Specifications:

Home page should mirror Google's minimalist design with centered search input
Use consistent spacing, typography, and color scheme
Display repository cards with essential information (name, description, stars, language, topics)
Sidebar should be collapsible and show topic counts
Selected topics should be visually distinct
Include smooth transitions and hover effects
Additional Features:

Display repository statistics (total count, filtered count)
Show user avatar and basic info once loaded
Implement search within repositories
Add sorting options (stars, updated date, name)
Provide complete, well-documented code with proper component structure, error handling, and performance optimizations. Include setup instructions and any necessary configuration files.