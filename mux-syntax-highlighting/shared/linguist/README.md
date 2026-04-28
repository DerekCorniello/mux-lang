# Linguist Upstream Contribution Checklist

## Prerequisites
- [ ] TextMate grammar (`textmate/source.mux.json`) complete
- [ ] Sample Mux files added to `shared/linguist/samples/`
- [ ] Language metadata defined below

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
3. Add `source.mux.json` to `lib/linguist/grammars/`
4. Add sample files to `samples/Mux/`
5. Submit PR to Linguist with description: "Add support for Mux programming language"
6. GitHub highlighting will be available after merge (latency: ~1-2 weeks)
