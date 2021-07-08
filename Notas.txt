      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    notas.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *----------------------------------------------------------------*
       SPECIAL-NAMES.
           CRT STATUS     CBL-KEY
           DECIMAL-POINT  IS COMMA.
      *----------------------------------------------------------------*
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
       COPY "TCKEYS.CPY".
       01  VARIAVEIS-WORKING.
           05  W-BRANCOS               PIC X(55) VALUE SPACES.
           05  W-ALUNO                 PIC X(50) VALUE SPACES.
           05  W-NOTA1                 PIC 9(03) VALUE ZEROS .
           05  W-NOTA2                 PIC 9(03) VALUE ZEROS .
           05  W-NOTA3                 PIC 9(03) VALUE ZEROS .
           05  W-NOTA4                 PIC 9(03) VALUE ZEROS .
           05  W-TOTAL-NOTAS           PIC 9(03) VALUE ZEROS .
           05  W-MEDIA                 PIC 9(02) VALUE ZEROS .
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
       00000-PRINCIPAL SECTION.
           PERFORM 10000-INICIA
           PERFORM 20000-PROCESSA
           PERFORM 90000-FINALIZA
          
           STOP RUN
           .
       99999-FIM-PRINCIPAL.
           EXIT.
      *----------------------------------------------------------------*
       10000-INICIA SECTION.
           
           INITIALIZE VARIAVEIS-WORKING
           
           DISPLAY "Informe o Aluno.:" 
           DISPLAY "Nota 1o Bimestre:" AT 0907
           DISPLAY "Nota 2o Bimestre:" AT 1007
           DISPLAY "Nota 3o Bimestre:" AT 1107
           DISPLAY "Nota 4o Bimestre:" AT 1207
           DISPLAY "Total das Notas.:" AT 1607
           DISPLAY "Media das Notas.:" AT 1707
           DISPLAY "Menssagem.......:" AT 2007
           
           .
       10000-FIM-INICIA.
           EXIT.
      *----------------------------------------------------------------*
       20000-PROCESSA SECTION.
           DISPLAY "Informe o nome do aluno"
                                       
           ACCEPT W-ALUNO              AT 0625
           DISPLAY W-BRANCOS           AT 2025
           IF KEY-ESC
              DISPLAY "Fim"            AT 2025
              STOP " "
              GO 89999-FIM-PROCESSA
           END-IF
           IF W-ALUNO = SPACES
              DISPLAY "Campo obrigatorio"
                                       AT 2025
              STOP " "
              DISPLAY W-BRANCOS        AT 2025
              GO 20000-PROCESSA
           END-IF
           .
       20100-ENTRA-NOTA-1.
           DISPLAY "Informe a primeira nota do aluno"
                                       AT 2025
           ACCEPT W-NOTA1              AT 0925
           DISPLAY W-BRANCOS           AT 2025
           IF KEY-ESC
              GO 20000-PROCESSA
           END-IF
           
           ADD W-NOTA1                 TO W-TOTAL-NOTAS
           DISPLAY W-TOTAL-NOTAS       AT 1625
           .
       20200-ENTRA-NOTA-2.
           DISPLAY "Informe a segunda nota do aluno"
                                       AT 2025
           ACCEPT W-NOTA2              AT 1025
           DISPLAY W-BRANCOS           AT 2025
           
           ADD W-NOTA2                 TO W-TOTAL-NOTAS
           DISPLAY W-TOTAL-NOTAS       AT 1625
           .
       20300-ENTRA-NOTA-3.
           DISPLAY "Informe a terceira nota do aluno"
                                       AT 2025
           ACCEPT W-NOTA3              AT 1125
           DISPLAY W-BRANCOS           AT 2025
           
           ADD W-NOTA3                 TO W-TOTAL-NOTAS
           DISPLAY W-TOTAL-NOTAS       AT 1625
           .
       20400-ENTRA-NOTA-4.
           DISPLAY "Informe a primeira nota do aluno"
                                       AT 2025
           ACCEPT W-NOTA4              AT 1225
           DISPLAY W-BRANCOS           AT 2025
           
           ADD W-NOTA4                 TO W-TOTAL-NOTAS
           DISPLAY W-TOTAL-NOTAS       AT 1625
           .
       20500-CALCULA-MEDIA.
           DIVIDE W-TOTAL-NOTAS        BY 4
                                   GIVING W-MEDIA
           DISPLAY W-MEDIA             AT 1725
           
           IF W-MEDIA < 5,0
              DISPLAY "REPROVADO!!!"   AT 2025
           ELSE
              IF W-MEDIA < 7,0
                 DISPLAY "RECUPERACAO!!!"
                                       AT 2025
              ELSE
                 DISPLAY "APROVADO!!!" AT 2025
              END-IF
           END-IF
           STOP " "
           .
       89999-FIM-PROCESSA.
           EXIT.
      *----------------------------------------------------------------*
       90000-FINALIZA SECTION.
       99999-FIM-FINALIZE.
           EXIT.