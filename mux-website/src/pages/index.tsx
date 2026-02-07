import type {ReactNode} from 'react';
import {useState} from 'react';
import clsx from 'clsx';
import Link from '@docusaurus/Link';
import Layout from '@theme/Layout';
import Heading from '@theme/Heading';
import CodeBlock from '@theme/CodeBlock';
import {CopyIcon, CheckIcon} from '@site/src/components/CodeIcons';

import styles from './index.module.css';



function HomepageHeader() {
  const [copied, setCopied] = useState(false);

  const handleCopy = () => {
    navigator.clipboard.writeText('cargo install mux-lang');
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
            A Programming Language<br />For The People
          </Heading>
          <p className={styles.heroSubtitle}>
            Python's ease of use with a Rust-like type system and Go's simplicity.<br />
            Strong static typing, LLVM-powered performance, and reference-counted memory management.
          </p>
          
          {/* Install command bar - kept as is */}
          <div className={styles.installBar}>
            <code className={styles.installCommand}>
                cargo install mux-lang
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
          <p style={{fontSize: '0.875rem', color: 'var(--ifm-color-emphasis-600)', marginTop: '0.5rem'}}>
            Requires LLVM 17 and clang. Run <code>mux doctor</code> to verify your setup.
          </p>
          <div style={{marginTop: '1.5rem'}}>
            <Link
              className="button button--secondary"
              to="/docs/getting-started/quick-start">
              Read the Docs
            </Link>
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
            <Heading as="h2">Hello, Mux!</Heading>
            <p>Your first Mux program:</p>
            <CodeBlock title="hello.mux" className={styles.codeExample}>
{`func main() returns void {
    print("Hello, Mux!")
}`}
            </CodeBlock>
            <p>
              <Link to="/docs/getting-started/quick-start">Get Started</Link> with the full installation guide.
            </p>
          </div>
          <div className="col col--6">
            <Heading as="h2">Quick Examples</Heading>
            <p>Pattern matching with Result types:</p>
            <CodeBlock title="example.mux" className={styles.codeExample}>
{`match result {
    Ok(value) { print(value.to_string()) }
    Err(error) { print("Error: " + error) }
}`}
            </CodeBlock>
            <p>
              <Link to="/docs/language-guide/overview">Explore the Language Guide</Link> for more.
            </p>
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
        <Heading as="h2" style={{textAlign: 'center', marginBottom: '2.5rem'}}>
          Why Choose Mux?
        </Heading>
        <div className="features-grid">
          <div className="feature-card">
            <Heading as="h3">Simple & Readable</Heading>
            <p>
              Clean syntax with no semicolons, clear type inference using `auto`,
              and explicit error handling with `Result` and `Optional` types.
              Code that is easy to read and maintain.
            </p>
          </div>
          <div className="feature-card">
            <Heading as="h3">Type Safe</Heading>
            <p>
              Strong static typing catches errors at compile time. No implicit
              conversions, powerful generics with monomorphization, and
              compile-time exhaustiveness checking.
            </p>
          </div>
          <div className="feature-card">
            <Heading as="h3">Fast & Efficient</Heading>
            <p>
              Native performance through LLVM code generation. Reference-counted
              memory management provides safety without garbage collection
              pauses or complex ownership rules.
            </p>
          </div>
          <div className="feature-card">
            <Heading as="h3">Pattern Matching</Heading>
            <p>
              Expressive pattern matching with guards for elegant control flow.
              Match on enums, Optionals, Results, and primitives with complete
              type safety.
            </p>
          </div>
          <div className="feature-card">
            <Heading as="h3">Memory Safe</Heading>
            <p>
              Automatic memory management using reference counting. No manual
              allocation, no garbage collector, and deterministic cleanup
              without a complex borrow checker.
            </p>
          </div>
          <div className="feature-card">
            <Heading as="h3">Modern & Expressive</Heading>
            <p>
              Full-featured with generics, interfaces, tagged union enums,
              collection literals, and lambda functions. A modern language
              for modern development.
            </p>
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
            <CodeBlock title="error-handling.mux" className={styles.showcaseCode}>
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
            </CodeBlock>
          </div>
          <div className="col col--6">
            <Heading as="h3">Generics & Collections</Heading>
            <CodeBlock title="generics.mux" className={styles.showcaseCode}>
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
stack.push(100)`}
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
      <div className="container" style={{textAlign: 'center'}}>
        <Heading as="h2">Ready to Get Started?</Heading>
        <p style={{fontSize: '1.25rem', marginBottom: '2rem'}}>
          Explore the documentation and start building with Mux today.
        </p>
        <div className={styles.buttons}>
          <Link
            className="button button--primary button--lg"
            to="/docs/getting-started/quick-start">
            Get Started
          </Link>
          <Link
            className="button button--link button--lg"
            to="/docs/language-guide/overview">
            Language Guide
          </Link>
        </div>
        <p style={{marginTop: '2rem', fontSize: '0.875rem', color: 'var(--ifm-color-emphasis-600)'}}>
          Mux is open source and licensed under MIT.
        </p>
      </div>
    </section>
  );
}

export default function Home(): ReactNode {
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
