# Saved for Reddit

[![Build Status](https://travis-ci.com/JBlackN/saved-for-reddit.svg?token=UyUqK5Y7LhTWBHGCPQhz&branch=master)](https://travis-ci.com/JBlackN/saved-for-reddit)

[Reddit](https://www.reddit.com) umožňuje uživatelům ukládat příspěvky, ne však více než ~1000 najednou &ndash; poté začnou být staré položky promazávány, což je pro dlouhodobou archivaci nežádoucí. Tato aplikace poskytne přihlášeným uživatelům možnost načítání uložených příspěvků do vlastní databáze bez jakýchkoliv omezení a jejich následné prohlížení ve formě webové prezentace (stránky) s filtrováním.

## Požadavky

- [x] Přihlášení přes OAuth2.
- [x] Načtení všech dostupných uložených příspěvků z Reddit API (JSON).
- [x] Aktualizace databáze příspěvků (tj. zařazení těch, které tam ještě nejsou).
- [x] Prezentace uložených příspěvků.
- [x] Filtrování uložených příspěvků (alespoň dle subredditu =~ kategorie).
- [x] Export uložených příspěvků (JSON).

## Použité závislosti

- [Aeson](http://hackage.haskell.org/package/aeson)
- [Base64-ByteString](https://hackage.haskell.org/package/base64-bytestring)
- [BlazeHtml](https://hackage.haskell.org/package/blaze-html) + [BlazeMarkup](https://hackage.haskell.org/package/blaze-markup)
- [Clay](https://hackage.haskell.org/package/clay)
- [HTTP-Conduit](https://hackage.haskell.org/package/http-conduit)
- [Persistent](http://hackage.haskell.org/package/persistent) + [SQLite](https://www.sqlite.org/index.html) backend
- [Scotty](https://github.com/scotty-web/scotty) + [ScottyCookie](https://hackage.haskell.org/package/scotty-cookie)
- [UUID](https://hackage.haskell.org/package/uuid)
- [Yaml](https://hackage.haskell.org/package/yaml)

Více viz [package.yaml](package.yaml).

## Instalace a použití

Aplikace potřebuje `client ID` a `client secret`, které lze získat registrací na [https://ssl.reddit.com/prefs/apps/](https://ssl.reddit.com/prefs/apps/) a doplnit do souboru [config.yml](config.yml.template) (**viz instrukce níže**).

```bash
git clone https://github.com/JBlackN/saved-for-reddit.git  # stažení
cd saved-for-reddit

cp config.yml.template config.yml                          # konfigurace (inicializace)
vi config.yml                                              # konfigurace (úprava a doplnění)
stack build                                                # sestavení

stack test                                                 # testování
stack haddock                                              # dokumentace (sestavení)
xdg-open .stack-work/install/*/*/*/doc/index.html          # dokumentace (prohlížení)

stack exec saved-for-reddit-exe                            # spuštění
```
