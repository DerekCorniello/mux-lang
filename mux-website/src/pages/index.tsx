import type {ReactNode} from 'react';
import clsx from 'clsx';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import Layout from '@theme/Layout';
import Heading from '@theme/Heading';

import styles from './index.module.css';

function HomepageHeader() {
  const {siteConfig} = useDocusaurusContext();
  return (
    <header className={clsx('hero', styles.heroBanner)}>
      <div className="container">
        <div className={styles.heroContent}>
          <div className={styles.logoContainer}>
            <img src="/img/mux-logo.png" alt="Mux Logo" className={styles.heroLogo} />
          </div>
          <Heading as="h1" className={styles.heroTitle}>
            A Programming Language<br />For The People
          </Heading>
          <p className={styles.heroSubtitle}>
            Python's ease of use with a Rust-like type system and Go's simplicity.<br />
            Strong static typing, LLVM-powered performance, and reference-counted memory management.
          </p>
          
          {/* Install command bar */}
          <div className={styles.installBar}>
            <code className={styles.installCommand}>
              git clone https://github.com/DerekCorniello/mux-lang && cd mux-lang && cargo build --release
            </code>
            <button 
              className={styles.copyButton}
              onClick={() => {
                navigator.clipboard.writeText('git clone https://github.com/DerekCorniello/mux-lang && cd mux-lang && cargo build --release');
              }}
              title="Copy to clipboard"
            >
              Copy
            </button>
          </div>
        </div>
      </div>
    </header>
  );
}

function QuickStartSection() {
  return (
    <section className={styles.quickStart}>
      <div className="container">
        <div className="row">
          <div className="col col--6">
            <Heading as="h2">Quick Start</Heading>
            <p>Install the Mux compiler and get started in minutes:</p>
            <pre className={styles.installCode}>
              <code>
{`# Clone the repository
git clone https://github.com/derekcorniello/mux-lang
cd mux-lang

# Build the compiler
cargo build --release

# Run your first program
cargo run -- run examples/hello.mux`}
              </code>
            </pre>
          </div>
          <div className="col col--6">
            <Heading as="h2">Hello, Mux!</Heading>
            <p>Your first Mux program:</p>
            <pre className={styles.codeExample}>
              <code>
{`func main() returns void {
    print("Hello, Mux!")
    
    auto numbers = [1, 2, 3, 4, 5]
    for num in numbers {
        print(num.to_string())
    }
}`}
              </code>
            </pre>
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
        <div className="row">
          <div className="col col--4">
            <div className="feature-card">
              <Heading as="h3">Simple Yet Powerful</Heading>
              <p>
                Clean, readable syntax inspired by modern languages. No semicolons,
                clear type inference with <code>auto</code>, and explicit error handling
                with <code>Result</code> and <code>Optional</code>.
              </p>
            </div>
          </div>
          <div className="col col--4">
            <div className="feature-card">
              <Heading as="h3">Strong Static Typing</Heading>
              <p>
                Catch errors at compile time with strict type checking.
                No implicit conversions, generics with monomorphization,
                and zero-cost abstractions.
              </p>
            </div>
          </div>
          <div className="col col--4">
            <div className="feature-card">
              <Heading as="h3">LLVM-Powered</Heading>
              <p>
                Native performance through LLVM code generation.
                Fast compilation and optimized runtime with
                reference-counted memory management.
              </p>
            </div>
          </div>
        </div>
        <div className="row" style={{marginTop: '2rem'}}>
          <div className="col col--4">
            <div className="feature-card">
              <Heading as="h3">Pattern Matching</Heading>
              <p>
                Rust-style pattern matching with guards for elegant control flow.
                Match on enums, optionals, results, and primitives with
                exhaustiveness checking.
              </p>
            </div>
          </div>
          <div className="col col--4">
            <div className="feature-card">
              <Heading as="h3">Memory Safe</Heading>
              <p>
                Automatic memory management through reference counting.
                No manual memory allocation, no garbage collection pauses,
                deterministic cleanup.
              </p>
            </div>
          </div>
          <div className="col col--4">
            <div className="feature-card">
              <Heading as="h3">Modern Features</Heading>
              <p>
                Generics, interfaces, enums as tagged unions, collection literals,
                lambda functions, and a comprehensive standard library.
              </p>
            </div>
          </div>
        </div>
      </div>
    </section>
  );
}

function CodeExampleSection() {
  return (
    <section className={styles.codeShowcase}>
      <div className="container">
        <Heading as="h2" style={{textAlign: 'center', marginBottom: '3rem'}}>
          See Mux In Action
        </Heading>
        <div className="row">
          <div className="col col--6">
            <Heading as="h3">Type-Safe Error Handling</Heading>
            <pre className={styles.showcaseCode}>
              <code>
{`func divide(int a, int b) returns Result<int, string> {
    if b == 0 {
        return Err("division by zero")
    }
    return Ok(a / b)
}

auto result = divide(10, 2)
match result {
    Ok(value) {
        print("Result: " + value.to_string())
    }
    Err(error) {
        print("Error: " + error)
    }
}`}
              </code>
            </pre>
          </div>
          <div className="col col--6">
            <Heading as="h3">Generics & Collections</Heading>
            <pre className={styles.showcaseCode}>
              <code>
{`class Stack<T> {
    list<T> items
    
    func push(T item) returns void {
        self.items.push_back(item)
    }
    
    func pop() returns Optional<T> {
        if self.items.is_empty() {
            return None
        }
        return self.items.pop_back()
    }
}

auto stack = Stack<int>.new()
stack.push(42)`}
              </code>
            </pre>
          </div>
        </div>
      </div>
    </section>
  );
}

function CTASection() {
  return (
    <section className={styles.cta}>
      <div className="container" style={{textAlign: 'center'}}>
        <Heading as="h2">Ready to Get Started?</Heading>
        <p style={{fontSize: '1.25rem', marginBottom: '2rem'}}>
          Explore the documentation and start building with Mux today.
        </p>
        <div className={styles.buttons}>
          <Link
            className="button button--primary button--lg"
            to="/docs/quick-start">
            Get Started
          </Link>
          <Link
            className="button button--secondary button--lg"
            to="/docs/examples">
            View Examples
          </Link>
        </div>
        <p style={{marginTop: '2rem', fontSize: '0.875rem', color: 'var(--ifm-color-emphasis-600)'}}>
          Mux is open source and licensed under MIT. Contributions welcome!
        </p>
      </div>
    </section>
  );
}

export default function Home(): ReactNode {
  const {siteConfig} = useDocusaurusContext();
  return (
    <Layout
      title="A Programming Language For The People"
      description="Mux is a statically-typed, reference-counted programming language combining Go-like minimalism with Rust-inspired safety">
      <HomepageHeader />
      <main>
        <QuickStartSection />
        <FeaturesSection />
        <CodeExampleSection />
        <CTASection />
      </main>
    </Layout>
  );
}
