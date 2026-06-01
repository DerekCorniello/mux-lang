# Mux Website Developer Improvements Plan

## Overview
Enhance Mux's discoverability through improved syntax highlighting, an interactive playground, curated code examples, and an accessible tour of the language.

---

## Phase 1: Shiki Syntax Highlighting Migration

### Goal
Replace `prism-react-renderer` with Shiki for higher quality syntax highlighting across all code blocks.

### Tasks
1. Install Shiki packages (`shiki`, `@shikijs/markdown-it` or similar)
2. Create Mux language definition using patterns from `mux-syntax-highlighting/shared/syntax-matrix.json`
3. Configure Shiki in Docusaurus markdown processing
4. Update `/src/theme/CodeBlock/index.tsx` to use Shiki for highlighting
5. Maintain all existing features:
   - Terminal window styling (macOS-style dots, file title header)
   - Line numbers support
   - Copy-to-clipboard button
6. Update all code blocks on the website:
   - Landing page (`/src/pages/index.tsx`)
   - All docs pages in `docs/`
   - Examples page

### Files to modify
- `mux-website/package.json` - Add Shiki dependencies
- `mux-website/docusaurus.config.ts` - Configure Shiki
- `mux-website/src/theme/CodeBlock/index.tsx` - Shiki integration
- `mux-website/docs/**/*.md` - Update code blocks as needed

### Verification
- All code blocks show proper syntax highlighting for Mux
- Dark/light mode switching works correctly
- Terminal styling preserved

---

## Phase 2: Playground Page

### Goal
Create a full-featured interactive code playground for running Mux code in the browser.

### Architecture

#### Centralized Runner API (`/src/lib/runner.ts`)
```typescript
interface RunnerRequest {
  code: string;
  timeout?: number; // ms
}

interface RunnerResponse {
  stdout: string;
  stderr: string;
  exitCode: number;
  executionTime: number;
  error?: string;
}

// Single source of truth for all playground/code execution
export async function runMuxCode(request: RunnerRequest): Promise<RunnerResponse>
```

This abstraction allows swapping the backend (Cloudflare Worker, Lambda, etc.) with minimal changes.

#### Cloudflare Worker Backend (`infra/cloudflare/playground-worker/`)
- Simple worker that receives code, executes it, returns results
- Memory and time limits enforced
- Sandbox isolation via WASM or process limits

**Note**: Implement UI first with mock/stub responses, then wire up the actual worker.

#### Playground Page (`/src/pages/playground.tsx`)
- Monaco Editor (VS Code's editor) for code input
- Split view: code editor | output panel
- Run button with keyboard shortcut (Cmd/Ctrl+Enter)
- Status bar showing execution time
- Example code dropdown/buttons
- Responsive layout for mobile

#### Embedded Playground Component (`/src/components/Playground/`)
- Reusable component for inline code editing
- Used in Tour of Mux and Examples pages
- Props: `initialCode`, `title`, `showOutput`

### Tasks
2.1. Create `/src/lib/runner.ts` with centralized runner interface
2.2. Create playground page at `/src/pages/playground.tsx`
2.3. Create embedded `<Playground>` React component
2.4. Add Monaco Editor integration
2.5. Create Cloudflare Worker backend
2.6. Wire up playground to Cloudflare Worker
2.7. Add "Playground" link to navbar

### Files to create/modify
- `mux-website/src/lib/runner.ts` - Runner abstraction
- `mux-website/src/pages/playground.tsx` - Playground page
- `mux-website/src/components/Playground/index.tsx` - Reusable component
- `mux-website/docusaurus.config.ts` - Add playground to nav
- `infra/cloudflare/playground-worker/` - Worker implementation

---

## Phase 3: Code Examples Page

### Goal
Curated collection of Mux code examples organized by category, with links to the playground.

### Structure (`/docs/examples/index.md` or `/src/pages/examples.tsx`)
- Categories:
  - **Getting Started**: Hello World, Variables, Functions
  - **Control Flow**: If/Else, Match, Loops
  - **Collections**: Lists, Maps, Sets, Ranges
  - **Types**: Classes, Enums, Interfaces, Generics
  - **Error Handling**: Result, Optional, Custom Errors
  - **Patterns**: Common idioms and best practices
  - **Real-World**: File I/O, HTTP requests, etc.

Each example includes:
- Runnable code block
- Brief explanation
- "Try in Playground" button

### Tasks
3.1. Create examples page/index
3.2. Add embedded playground for each example
3.3. Add "Examples" link to navbar

### Files to create/modify
- `mux-website/docs/examples/index.md` or `mux-website/src/pages/examples.tsx`
- `mux-website/docusaurus.config.ts` - Add Examples to nav

---

## Phase 4: Tour of Mux

### Goal
A progressive, example-driven tour of Mux with many small, focused examples and video placeholders.

### Structure
Short pages with ONE focused example each. The example can contain multiple elements (e.g., variables, functions, prints), but should demonstrate one concept clearly. Many pages, not a few long ones.

#### Tour Pages
1. **Introduction** (`/tour/`)
   - What is Mux?
   - Why use it?
   - First look at code

2. **Hello World** (`/tour/hello-world`)
   - Simplest program
   - print() function
   - Comments

3. **Variables** (`/tour/variables`)
   - Explicit types
   - Auto inference
   - Constants

4. **Basic Types** (`/tour/basic-types`)
   - int, float, bool, string
   - Type conversions

5. **Functions** (`/tour/functions`)
   - Function declaration
   - Parameters and returns
   - Default arguments

6. **Control Flow** (`/tour/control-flow`)
   - If/else
   - While loops
   - For loops

7. **Pattern Matching** (`/tour/pattern-matching`)
   - Match expressions
   - Enum patterns
   - Guard clauses

8. **Lists** (`/tour/lists`)
   - List literals
   - push_back, pop_back
   - Iteration

9. **Maps** (`/tour/maps`)
   - Map literals
   - get, set
   - Iteration

10. **Sets** (`/tour/sets`)
    - Set literals
    - add, remove
    - Set operations

11. **Classes** (`/tour/classes`)
    - Class declaration
    - Methods
    - Built-in .new() plus factory methods (from/with_*)

12. **Enums** (`/tour/enums`)
    - Enum declaration
    - Variants with data
    - Matching enums

13. **Generics** (`/tour/generics`)
    - Generic functions
    - Generic classes
    - Type constraints

14. **Error Handling** (`/tour/error-handling`)
    - Result types
    - Optional types
    - Pattern matching errors

15. **Interfaces** (`/tour/interfaces`)
    - Interface declaration
    - Implementing interfaces
    - Traits with `is`

16. **References** (`/tour/references`)
    - Reference parameters
    - Dereferencing

17. **Modules** (`/tour/modules`)
    - Import statements
    - Standard library
    - Creating modules

18. **Next Steps** (`/tour/next-steps`)
    - Links to full docs
    - Community links
    - Contributing

### Video Placeholder Component
```tsx
// Simple black rectangle with centered text
function VideoPlaceholder({ topic }: { topic: string }) {
  return (
    <div style={{
      backgroundColor: '#000',
      color: '#fff',
      padding: '60px 20px',
      textAlign: 'center',
      borderRadius: '8px',
      margin: '2rem 0'
    }}>
      <p style={{ fontSize: '1.25rem', fontWeight: 500 }}>
        Video for {topic} are in the works!
      </p>
    </div>
  );
}
```

### Tasks
4.1. Create `VideoPlaceholder` component
4.2. Create tour index page
4.3. Create 18 tour pages (ONE focused example each)
4.4. Add embedded playground to each page (DONE)
4.5. Add video placeholder at the start of each page
4.6. Add "Tour" to navbar

### Files to create/modify
- `mux-website/src/components/VideoPlaceholder/index.tsx`
- `mux-website/docs/tour/index.md`
- `mux-website/docs/tour/hello-world.md`
- `mux-website/docs/tour/variables.md`
- `mux-website/docs/tour/basic-types.md`
- `mux-website/docs/tour/functions.md`
- `mux-website/docs/tour/control-flow.md`
- `mux-website/docs/tour/pattern-matching.md`
- `mux-website/docs/tour/lists.md`
- `mux-website/docs/tour/maps.md`
- `mux-website/docs/tour/sets.md`
- `mux-website/docs/tour/classes.md`
- `mux-website/docs/tour/enums.md`
- `mux-website/docs/tour/generics.md`
- `mux-website/docs/tour/error-handling.md`
- `mux-website/docs/tour/interfaces.md`
- `mux-website/docs/tour/references.md`
- `mux-website/docs/tour/modules.md`
- `mux-website/docs/tour/next-steps.md`
- `mux-website/sidebars.ts` - Add tour sidebar
- `mux-website/docusaurus.config.ts` - Add Tour to nav

---

## Implementation Order

1. **Phase 1** - Shiki Syntax Highlighting (foundational)
2. **Phase 2** - Playground UI (with mock runner)
3. **Phase 3** - Code Examples Page
4. **Phase 4** - Tour of Mux (many pages)
5. **Phase 2 continued** - Cloudflare Worker backend

---

## Design Guidelines

### Code Block Styling
- Terminal window appearance (macOS-style colored dots)
- File name in header bar
- Copy button with checkmark feedback
- Optional line numbers
- Shiki-powered syntax highlighting

### Video Placeholder
- Pure black rectangle (#000000)
- White text, centered
- Rounded corners (8px)
- Subtle margin above/below

### Playground
- Monaco Editor for code input
- Split panel: editor | output
- Dark theme matching website
- Run button with keyboard shortcut
- Status bar with execution time

### Tour Pages
- 2-3 focused examples per page
- Embedded playground for each example
- Video placeholder at top
- "Next" and "Previous" navigation
- Progress indicator in sidebar

---

## Success Criteria
- All code blocks show proper Mux syntax highlighting
- Playground executes code and displays output
- Tour of Mux has 18+ pages with small, focused examples
- Video placeholders appear at the start of each tour section
- Navbar includes: Documentation, Quick Start, Tour, Examples, Playground, Search
- All pages work in both light and dark modes
