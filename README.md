# Saved for Reddit

[Reddit](https://www.reddit.com) umožňuje uživatelům ukládat příspěvky, ne však více než ~1000 najednou &ndash; poté začnou být staré položky promazávány, což je pro dlouhodobou archivaci nežádoucí. Tato aplikace poskytne přihlášeným uživatelům možnost načítání uložených příspěvků do vlastní databáze bez jakýchkoliv omezení a jejich následné prohlížení ve formě webové prezentace (stránky) s filtrováním.

## Požadavky

- [ ] Přihlášení přes OAuth2.
- [ ] Načtení všech dostupných uložených příspěvků z Reddit API (JSON).
- [ ] Aktualizace databáze příspěvků (tj. zařazení těch, které tam ještě nejsou).
- [ ] Prezentace uložených příspěvků.
- [ ] Filtrování uložených příspěvků (alespoň dle subredditu =~ kategorie).
- [ ] Export uložených příspěvků (JSON).

## Použité knihovny a technologie

Pravděpodobně (výčet prozatím neúplný):

- [SQLite](https://www.sqlite.org/index.html)
- [Scotty](https://github.com/scotty-web/scotty)
- [hoauth2](https://hackage.haskell.org/package/hoauth2)
- [aeson](https://github.com/bos/aeson)
