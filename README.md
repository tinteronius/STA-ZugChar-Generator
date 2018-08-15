STA-ZugChar-Generator
======

Mehrstufige Skripte zur Ermittlung passender Charakteristiken für die STA.

1. **newA-V-MitLaufwegen.R**
   Fahrlagen auf STA hinreichend eindeutig mappen
   (optional) => result_detail_v9 (ordner)

2. **bottomUp_new_a(v).R**
   Ermittlung 90%-Char
   Große LIste mit allen Zugcharakteristiken
   Für jeden STA die ZugCh, die 90% abdecken

3. **setCoveringOptimizer.R**
   Beste Überdeckung für 8...500 ZugCh => Ordner bottom_up
   aus allen ZugCh netzweit
   Es wird eine Range aufgemacht, um später zu gucken, wie viele schnelle man braucht

4. **GainMaximizer.R**
   Kombinationen durchgehen: von Kombinatorik 
   -> Ordner optimizedTrains
   -> Entscheidung für eine REM

5. **SystemtrassenAssigner.R**
   Welche Systemtrasse ist es jetzt wirklich?
   Zuordnung STA mit ZugCh

6. **Tagesgang.R**
   => Input der für die STA
   -> Ordner filled_table 
   
=======

