# Emacs Keybinding Conventions

## Übersicht der Präfix-Konventionen

---

## `C-` (Control)

Einzelne `C-` Bindings sind für **häufig genutzte, grundlegende Befehle** reserviert.

| Beispiel        | Bedeutung                        |
|-----------------|----------------------------------|
| `C-f`, `C-b`    | Zeichenweise vorwärts/rückwärts  |
| `C-n`, `C-p`    | Zeile rauf/runter                |
| `C-a`, `C-e`    | Zeilenanfang/-ende               |
| `C-k`           | Zeile löschen (kill)             |
| `C-y`           | Yank (paste)                     |
| `C-s`, `C-r`    | Suche vorwärts/rückwärts         |
| `C-g`           | Abbrechen (quit/cancel)          |
| `C-SPC`         | Mark setzen                      |

**Konvention:** Nur für fundamentale Editierbefehle, die extrem oft gebraucht werden. Eigene Packages sollten hier **nicht** binden (zu wenig Namespace-Raum).

---

## `C-x` (Control + x — "Extended" Befehle)

`C-x` ist der **globale Emacs-Präfix** für systemweite, sitzungsbezogene Aktionen.

| Beispiel        | Bedeutung                                |
|-----------------|------------------------------------------|
| `C-x C-f`       | Datei öffnen (find-file)                 |
| `C-x C-s`       | Datei speichern                          |
| `C-x C-c`       | Emacs beenden                            |
| `C-x b`         | Buffer wechseln                          |
| `C-x k`         | Buffer schließen                         |
| `C-x o`         | Anderes Fenster (other-window)           |
| `C-x 0/1/2/3`   | Fenster-Layouts                          |
| `C-x C-b`       | Buffer-Liste anzeigen                    |
| `C-x r ...`     | Register & Rectangle-Befehle            |
| `C-x n ...`     | Narrowing-Befehle                        |
| `C-x p ...`     | Project-Befehle (project.el)             |
| `C-x 8 ...`     | Unicode-Zeichen einfügen                 |

**Konvention:**
- Für **globale, sitzungsweite** Operationen
- Emacs-Kern nutzt diesen Präfix stark → eigene Bindings hier **sparsam** vergeben
- `C-x C-<letter>` = wichtige Datei-/Buffer-Operationen
- `C-x <letter>` = weniger kritische globale Operationen

---

## `C-c` (Control + c — "Custom/Major-Mode" Befehle)

`C-c` ist **reserviert für Nutzer und Major-Modes**.

| Bereich                  | Konvention                                                   |
|--------------------------|--------------------------------------------------------------|
| `C-c C-<letter>`         | **Major-Mode-spezifische** Befehle (z.B. `C-c C-c` in org)  |
| `C-c <letter>`           | **Nutzer-eigene** globale Bindings                           |
| `C-c C-c`                | "Ausführen" / "Bestätigen" in vielen Modes                   |
| `C-c C-k`                | "Abbrechen" in vielen Modes                                  |
| `C-c C-d`                | Dokumentation (z.B. CIDER, LSP)                              |
| `C-c C-z`                | Zum REPL wechseln (Clojure, Python, etc.)                    |

**Konvention (laut GNU-Emacs-Standard):**
- `C-c C-<letter>` → **Major-Mode** darf hier binden
- `C-c <letter>` (ohne zweites Control) → **nur Nutzer**, Packages dürfen das NICHT verwenden
- `C-c <Zahl>` / `C-c <Sonderzeichen>` → ebenfalls für Nutzer reserviert

---

## `M-` (Meta / Alt)

`M-` Befehle arbeiten typischerweise auf **größeren Einheiten** als `C-`.

| Beispiel        | Bedeutung                          |
|-----------------|------------------------------------|
| `M-f`, `M-b`    | Wortweise vorwärts/rückwärts       |
| `M-d`           | Wort löschen                       |
| `M-w`           | Copy (kill-ring-save)              |
| `M-y`           | Yank-pop (vorherige Kopie einfügen)|
| `M-<`, `M->`    | Anfang/Ende des Buffers            |
| `M-g g`         | Zu Zeile springen (goto-line)      |
| `M-x`           | Befehl eingeben (execute-extended) |
| `M-:`           | Elisp auswerten                    |
| `M-;`           | Kommentar einfügen/togglen         |
| `M-q`           | Absatz umbrechen (fill-paragraph)  |
| `M-/`           | Dabbrev-expand (Auto-Vervollständigung) |
| `M-!`           | Shell-Befehl ausführen             |
| `M-|`           | Shell-Befehl auf Region anwenden   |

**Konvention:** `M-` = eine Abstraktionsebene höher als `C-`. Wo `C-f` ein Zeichen, geht `M-f` ein Wort.

---

## `C-M-` (Control + Meta — "Structural" Befehle)

`C-M-` ist für **strukturelle / syntaktische** Operationen, besonders bei Code.

| Beispiel        | Bedeutung                                   |
|-----------------|---------------------------------------------|
| `C-M-f`, `C-M-b`| S-Expression vorwärts/rückwärts (sexp)      |
| `C-M-n`, `C-M-p`| Liste vorwärts/rückwärts                    |
| `C-M-u`, `C-M-d`| Übergeordnete/untergeordnete Sexp           |
| `C-M-k`         | S-Expression löschen                        |
| `C-M-SPC`       | S-Expression markieren                      |
| `C-M-a`, `C-M-e`| Funktionsanfang/-ende                       |
| `C-M-q`         | Einrückung neu formatieren                  |
| `C-M-h`         | Funktion markieren                          |
| `C-M-\`         | Region einrücken (indent-region)            |
| `C-M-x`         | Top-Level-Form auswerten (in Lisps)         |

**Konvention:** `C-M-` = strukturelle Bewegung/Manipulation. Sinnvoll vor allem in Programmier-Modes (Lisp, Clojure, etc.).

---

## `s-` (Super / Windows-Taste)

Die Super-Taste (`s-`) ist **nicht durch Emacs-Standard belegt** → ideal für eigene globale Bindings.

**Konvention:**
- Eigene, globale Shortcuts die zu keinem Mode gehören
- Fensterverwaltung, App-Launcher-ähnliche Aktionen
- Oft genutzt für: `s-<letter>` = häufig genutzte eigene Befehle

---

## `H-` (Hyper)

Noch freier als Super. Auf modernen Tastaturen selten direkt vorhanden (oft über `xmodmap` konfiguriert).

---

## Zusammenfassung: Für wen ist was?

| Präfix        | Für wen?                            | Typischer Inhalt                            |
|---------------|-------------------------------------|---------------------------------------------|
| `C-`          | Emacs-Kern                          | Fundamentale Editierbefehle                 |
| `C-x`         | Emacs-Kern + globale Extras         | Sitzungs-/systemweite Operationen           |
| `C-c C-<key>` | Major-Modes / Packages              | Mode-spezifische Aktionen                   |
| `C-c <key>`   | **Nur Nutzer** (Packages verboten!) | Persönliche globale Shortcuts               |
| `M-`          | Emacs-Kern                          | Wort-/Absatz-Level-Operationen              |
| `C-M-`        | Emacs-Kern + Sprach-Modes           | Strukturelle/syntaktische Operationen       |
| `s-`          | Nutzer / eigene Config              | Globale Custom-Shortcuts                    |
| `H-`          | Nutzer / eigene Config              | Globale Custom-Shortcuts (noch freier)      |

---

## Praktische Faustregel für eigene Keybindings

```
Global + häufig       → s-<key>
Global + gelegentlich → C-c <key>        (nur ein Buchstabe, kein zweites C-)
Mode-spezifisch       → C-c C-<key>      (Mode darf hier binden)
Strukturell (Code)    → C-M-<key>
```

---

## Quellen

- [GNU Emacs Manual: Key Binding Conventions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html)
- [GNU Emacs Manual: Keymaps](https://www.gnu.org/software/emacs/manual/html_node/elisp/Keymaps.html)
