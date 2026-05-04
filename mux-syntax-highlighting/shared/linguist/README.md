# Linguist Upstream Contribution Checklist

## Prerequisites
- [ ] TextMate grammar generation works from `shared/syntax-matrix.json`
- [ ] Sample Mux files added to `shared/linguist/samples/`
- [ ] Language metadata defined below
- [ ] `node ../../scripts/check-parity.js` passes

## Language Metadata (for Linguist PR)
```yaml
- name: Mux
  type: programming
  extensions:
    - ".mux"
  aliases:
    - mux
  color: "#FF6B6B"
  ace_mode: textmate
  code_mirror_mode: text/x-mux
  language_id: <assigned by Linguist>
  source: https://github.com/DerekCorniello/mux-lang
```

## Steps to Contribute
1. Fork https://github.com/github/linguist
2. Add language entry to `lib/linguist/languages.yml`
3. Add the generated TextMate grammar to `lib/linguist/grammars/`
4. Add sample files to `samples/Mux/`
5. Submit PR to Linguist with description: "Add support for Mux programming language"
6. GitHub highlighting will be available after merge (latency: ~1-2 weeks)

## Shared Validation
- Run `node ../../scripts/check-parity.js` before updating samples or grammar artifacts.
- Keep `shared/syntax-matrix.json` in sync with both editor grammars.
