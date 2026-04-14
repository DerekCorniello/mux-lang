import type {ReactNode} from 'react';
import {useState} from 'react';
import clsx from 'clsx';
import Link from '@docusaurus/Link';
import Layout from '@theme/Layout';
import Heading from '@theme/Heading';
import CodeBlock from '@theme/CodeBlock';
import {CopyIcon, CheckIcon} from '@site/src/components/CodeIcons';

import styles from './index.module.css';

// Feature icons as simple SVG components
const features = [
  {
    icon: (
      <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round">
        <path d="M12 2L2 7l10 5 10-5-10-5z"/>
        <path d="M2 17l10 5 10-5"/>
        <path d="M2 12l10 5 10-5"/>
      </svg>
    ),
    title: 'Simple & Intuitive',
    description: 'Clean syntax without semicolons, Python-like readability with Go-inspired simplicity. Write code that is easy to understand and maintain.'
  },
  {
    icon: (
      <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round">
        <path d="M12 22s8-4 8-10V5l-8-3-8 3v7c0 6 8 10 8 10z"/>
        <path d="M9 12l2 2 4-4"/>
      </svg>
    ),
    title: 'Type Safe',
    description: 'Strong static typing with no implicit conversions. Catch errors at compile time with powerful generics and exhaustive pattern matching.'
  },
  {
    icon: (
      <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round">
        <polygon points="13 2 3 14 12 14 11 22 21 10 12 10 13 2"/>
      </svg>
    ),
    title: 'Fast & Native',
    description: 'LLVM-powered compilation delivers native performance. Reference-counted memory management provides safety without GC pauses or complex ownership.'
  },
  {
    icon: (
      <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round">
        <rect x="3" y="3" width="18" height="18" rx="2" ry="2"/>
        <line x1="3" y1="9" x2="21" y2="9"/>
        <line x1="9" y1="21" x2="9" y2="9"/>
      </svg>
    ),
    title: 'Pattern Matching',
    description: 'Expressive match expressions with guards for enums, Result types, and Optional types. Exhaustiveness checking ensures you handle all cases.'
  },
  {
    icon: (
      <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round">
        <path d="M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z"/>
        <polyline points="3.27 6.96 12 12.01 20.73 6.96"/>
        <line x1="12" y1="22.08" x2="12" y2="12"/>
      </svg>
    ),
    title: 'Modern Features',
    description: 'Full-featured with generics, interfaces, tagged unions, collection literals, and first-class functions. Everything you need for modern development.'
  },
  {
    icon: (
      <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round">
        <path d="M17 21v-2a4 4 0 0 0-4-4H5a4 4 0 0 0-4 4v2"/>
        <circle cx="9" cy="7" r="4"/>
        <path d="M23 21v-2a4 4 0 0 0-3-3.87"/>
        <path d="M16 3.13a4 4 0 0 1 0 7.75"/>
      </svg>
    ),
    title: 'Developer Friendly',
    description: 'Built for humans with helpful error messages, built-in tooling, and comprehensive documentation. Start writing code in minutes, not hours.'
  }
];

function HomepageHeader() {
  const [copied, setCopied] = useState(false);

  const handleCopy = () => {
    navigator.clipboard.writeText('curl -fsSL https://raw.githubusercontent.com/DerekCorniello/mux-lang/main/scripts/install.sh | sh');
    setCopied(true);
    setTimeout(() => setCopied(false), 2000);
  };

  return (
    <header className={clsx('hero', styles.heroBanner)}>
      <div className="container">
        <div className={styles.heroContent}>
          <div className={styles.logoContainer}>
            <img src="/img/mux-logo.png" alt="Mux Logo" className={styles.heroLogo} />
          </div>
          <Heading as="h1" className={styles.heroTitle}>
            The Programming Language<br />
            <span className={styles.heroTitleHighlight}>For Everyone</span>
          </Heading>
          <p className={styles.heroSubtitle}>
            Mux combines Python's readability, Go's simplicity, and Rust's type safety
            into one powerful language. Write fast, safe, and maintainable code
            without the complexity.
          </p>
          
          {/* Install command bar */}
          <div className={styles.installBar}>
            <code className={styles.installCommand}>
              curl -fsSL https://raw.githubusercontent.com/DerekCorniello/mux-lang/main/scripts/install.sh | sh
            </code>
            <button 
              className={styles.copyButton}
              onClick={handleCopy}
              title={copied ? "Copied!" : "Copy to clipboard"}
              type="button"
            >
              {copied ? <CheckIcon /> : <CopyIcon />}
            </button>
          </div>
          
          <p className={styles.installNote}>
            Quick install for Linux & macOS. 
            <Link to="/docs/getting-started/quick-start" className={styles.installLink}>
              Other options →
            </Link>
          </p>
          
          <div className={styles.heroButtons}>
            <Link
              className={clsx('button', 'button--primary', 'button--lg', styles.ctaButton)}
              to="/docs/getting-started/quick-start">
              Get Started
            </Link>
            <Link
              className={clsx('button', 'button--secondary', 'button--lg', styles.secondaryButton)}
              to="https://github.com/DerekCorniello/mux-lang">
              View on GitHub
            </Link>
          </div>
          
          <div className={styles.badges}>
            <span className={styles.badge}>v0.2.0</span>
            <span className={styles.badge}>MIT Licensed</span>
            <span className={styles.badge}>Open Source</span>
          </div>
        </div>
      </div>
    </header>
  );
}

function InstallationSection() {
  return (
    <section className={styles.installation}>
      <div className="container">
        <Heading as="h2" className={styles.sectionTitle}>
          Installation
        </Heading>
        <p className={styles.sectionSubtitle}>
          Choose the method that works best for you
        </p>
        
        <div className={styles.installGrid}>
          <div className={styles.installCard}>
            <div className={styles.installHeader}>
              <span className={styles.installIcon}>🚀</span>
              <Heading as="h3">Prebuilt Binaries</Heading>
              <span className={styles.recommendedBadge}>Recommended</span>
            </div>
            <p>Fastest way to get started. No Rust or LLVM required.</p>
            <CodeBlock className={styles.installCode}>
{`# Linux & macOS
curl -fsSL https://raw.githubusercontent.com/DerekCorniello/mux-lang/main/scripts/install.sh | sh

# Windows PowerShell
iwr -useb https://raw.githubusercontent.com/DerekCorniello/mux-lang/main/scripts/install.ps1 | iex`}
            </CodeBlock>
          </div>
          
          <div className={styles.installCard}>
            <div className={styles.installHeader}>
              <span className={styles.installIcon}>📦</span>
              <Heading as="h3">From crates.io</Heading>
            </div>
            <p>For Rust developers who already have cargo installed.</p>
            <CodeBlock className={styles.installCode}>
{`cargo install mux-lang

# Verify installation
mux doctor`}
            </CodeBlock>
          </div>
          
          <div className={styles.installCard}>
            <div className={styles.installHeader}>
              <span className={styles.installIcon}>🔧</span>
              <Heading as="h3">Build from Source</Heading>
            </div>
            <p>For contributors and those who want the latest features.</p>
            <CodeBlock className={styles.installCode}>
{`git clone https://github.com/DerekCorniello/mux-lang
cd mux-lang
./scripts/bootstrap-dev.sh
./scripts/dev-cargo.sh build`}
            </CodeBlock>
          </div>
        </div>
      </div>
    </section>
  );
}

function QuickStartSection() {
  return (
    <section className={styles.quickStart}>
      <div className="container">
        <div className={styles.quickStartGrid}>
          <div className={styles.quickStartContent}>
            <Heading as="h2">Write Your First Program</Heading>
            <p className={styles.quickStartDescription}>
              Mux is designed to be instantly familiar. Here is a complete program 
              that demonstrates error handling, pattern matching, and type safety.
            </p>
            <div className={styles.quickStartSteps}>
              <div className={styles.step}>
                <span className={styles.stepNumber}>1</span>
                <span>Create a file called <code>hello.mux</code></span>
              </div>
              <div className={styles.step}>
                <span className={styles.stepNumber}>2</span>
                <span>Write your code</span>
              </div>
              <div className={styles.step}>
                <span className={styles.stepNumber}>3</span>
                <span>Run with <code>mux run hello.mux</code></span>
              </div>
            </div>
            <Link
              className={clsx('button', 'button--primary')}
              to="/docs/getting-started/quick-start">
              Read the Quick Start Guide
            </Link>
          </div>
          
          <div className={styles.codeBlockWrapper}>
            <CodeBlock title="error-handling.mux" className={styles.featuredCode}>
{`func divide(int a, int b) returns result<int, string> {
    if b == 0 {
        return err("division by zero")
    }
    return ok(a / b)
}

func main() returns void {
    auto result = divide(10, 2)
    
    match result {
        ok(value) {
            print("Result: " + value.to_string())
        }
        err(error) {
            print("Error: " + error)
        }
    }
}`}
            </CodeBlock>
          </div>
        </div>
      </div>
    </section>
  );
}

function FeaturesSection() {
  return (
    <section className={styles.features}>
      <div className="container">
        <Heading as="h2" className={styles.sectionTitle}>
          Why Developers Love Mux
        </Heading>
        <p className={styles.sectionSubtitle}>
          Built with modern software development in mind
        </p>
        
        <div className={styles.featuresGrid}>
          {features.map((feature, idx) => (
            <div key={idx} className={styles.featureCard}>
              <div className={styles.featureIcon}>{feature.icon}</div>
              <Heading as="h3">{feature.title}</Heading>
              <p>{feature.description}</p>
            </div>
          ))}
        </div>
      </div>
    </section>
  );
}

function CodeExamplesSection() {
  return (
    <section className={styles.codeShowcase}>
      <div className="container">
        <Heading as="h2" className={styles.sectionTitle}>
          See Mux in Action
        </Heading>
        <p className={styles.sectionSubtitle}>
          Real code that showcases Mux's powerful features
        </p>
        
        <div className={styles.examplesGrid}>
          <div className={styles.exampleCard}>
            <Heading as="h3">Generics & Collections</Heading>
            <CodeBlock title="stack.mux" className={styles.exampleCode}>
{`class Stack<T> {
    list<T> items
    
    func push(T item) returns void {
        self.items.push_back(item)
    }
    
    func pop() returns optional<T> {
        if self.items.is_empty() {
            return none
        }
        return self.items.pop_back()
    }
    
    func peek() returns optional<T> {
        return self.items.get(self.items.size() - 1)
    }
}

auto stack = Stack<int>.new()
stack.push(42)
match stack.pop() {
    some(value) { print(value.to_string()) }
    none { print("Stack is empty") }
}`}
            </CodeBlock>
          </div>
          
          <div className={styles.exampleCard}>
            <Heading as="h3">Pattern Matching</Heading>
            <CodeBlock title="enums.mux" className={styles.exampleCode}>
{`enum Shape {
    Circle(float radius)
    Rectangle(float width, float height)
    Square(float size)
}

func area(Shape shape) returns float {
    match shape {
        Circle(r) { return 3.14159 * r * r }
        Rectangle(w, h) { return w * h }
        Square(s) { return s * s }
    }
}

auto shapes = [
    Circle.new(5.0),
    Rectangle.new(4.0, 6.0)
]

for shape in shapes {
    print(area(shape).to_string())
}`}
            </CodeBlock>
          </div>
        </div>
      </div>
    </section>
  );
}

function CTASection() {
  return (
    <section className={styles.cta}>
      <div className="container">
        <div className={styles.ctaContent}>
          <Heading as="h2">Ready to Start Building?</Heading>
          <p className={styles.ctaDescription}>
            Join the growing community of developers using Mux to build 
            fast, reliable, and maintainable applications.
          </p>
          <div className={styles.ctaButtons}>
            <Link
              className={clsx('button', 'button--primary', 'button--lg')}
              to="/docs/getting-started/quick-start">
              Get Started
            </Link>
            <Link
              className={clsx('button', 'button--secondary', 'button--lg')}
              to="/docs/language-guide/overview">
              Explore the Language
            </Link>
          </div>
          <div className={styles.ctaLinks}>
            <Link to="https://github.com/DerekCorniello/mux-lang">Star on GitHub</Link>
            <span className={styles.linkSeparator}>•</span>
            <Link to="https://github.com/DerekCorniello/mux-lang/discussions">Join Discussions</Link>
            <span className={styles.linkSeparator}>•</span>
            <Link to="https://github.com/DerekCorniello/mux-lang/issues">Report Issues</Link>
          </div>
        </div>
      </div>
    </section>
  );
}

export default function Home(): ReactNode {
  return (
    <Layout
      title="Mux - The Programming Language For Everyone"
      description="Mux is a statically-typed, reference-counted programming language combining Python's readability, Go's simplicity, and Rust's type safety">
      <HomepageHeader />
      <main>
        <InstallationSection />
        <QuickStartSection />
        <FeaturesSection />
        <CodeExamplesSection />
        <CTASection />
      </main>
    </Layout>
  );
}
