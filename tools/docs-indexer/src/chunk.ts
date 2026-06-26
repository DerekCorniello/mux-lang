/**
 * Token counts aren't computed exactly (no tokenizer dependency); ~4 chars
 * per token is a standard approximation for English prose and code fences,
 * which is precise enough for picking chunk boundaries.
 *
 * Chunks must fit the embedding model's context window (bge-base-en-v1.5 reads
 * at most 512 tokens), or the tail of a chunk never influences its vector and
 * becomes unfindable by search. The max target is held at ~450 tokens to leave
 * headroom for the query instruction prefix and for code, which is denser than
 * the 4-chars-per-token estimate.
 */
const CHARS_PER_TOKEN = 4;
const MIN_CHARS = 175 * CHARS_PER_TOKEN;
const MAX_CHARS = 450 * CHARS_PER_TOKEN;

export interface Chunk {
  text: string;
  /** Nearest heading above this chunk, if any. */
  heading: string | null;
}

interface Section {
  heading: string | null;
  text: string;
}

function splitIntoSections(content: string): Section[] {
  const lines = content.split('\n');
  const sections: Section[] = [];
  let currentHeading: string | null = null;
  let currentLines: string[] = [];

  const flush = () => {
    const text = currentLines.join('\n').trim();
    if (text) {
      sections.push({ heading: currentHeading, text });
    }
    currentLines = [];
  };

  for (const line of lines) {
    const headingMatch = /^(#{1,6})[ \t]+(\S.*)$/.exec(line);
    if (headingMatch) {
      flush();
      currentHeading = headingMatch[2].trim();
    }
    currentLines.push(line);
  }
  flush();

  return sections;
}

function splitOversizedSection(section: Section): Section[] {
  const paragraphs = section.text.split(/\n{2,}/);
  const result: Section[] = [];
  let buffer = '';

  for (const paragraph of paragraphs) {
    const candidate = buffer ? `${buffer}\n\n${paragraph}` : paragraph;
    if (candidate.length > MAX_CHARS && buffer) {
      result.push({ heading: section.heading, text: buffer });
      buffer = paragraph;
    } else {
      buffer = candidate;
    }
  }
  if (buffer) {
    result.push({ heading: section.heading, text: buffer });
  }

  return result;
}

export function chunkContent(content: string): Chunk[] {
  const rawSections = splitIntoSections(content).flatMap((section) =>
    section.text.length > MAX_CHARS ? splitOversizedSection(section) : [section],
  );

  const chunks: Chunk[] = [];
  let buffer: Section | null = null;

  for (const section of rawSections) {
    if (!buffer) {
      buffer = { ...section };
      continue;
    }

    const merged: string = `${buffer.text}\n\n${section.text}`;
    if (buffer.text.length < MIN_CHARS && merged.length <= MAX_CHARS) {
      buffer = { heading: buffer.heading, text: merged };
    } else {
      chunks.push({ text: buffer.text, heading: buffer.heading });
      buffer = { ...section };
    }
  }
  if (buffer) {
    chunks.push({ text: buffer.text, heading: buffer.heading });
  }

  return chunks;
}
