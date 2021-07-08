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
           DISPLAY "Nota 1o Bimestre:"
           DISPLAY "Nota 2o Bimestre:" 
           DISPLAY "Nota 3o Bimestre:" 
           DISPLAY "Nota 4o Bimestre:" 
           DISPLAY "Total das Notas.:" 
           DISPLAY "Media das Notas.:" 
           DISPLAY "Menssagem.......:" 
           
           .
       10000-FIM-INICIA.
           EXIT.
      *----------------------------------------------------------------*
       20000-PROCESSA SECTION.
           DISPLAY "Informe o nome do aluno"
                                       
           ACCEPT W-ALUNO              
           DISPLAY W-BRANCOS          
           IF KEY-ESC
              DISPLAY "Fim"            
              STOP " "
              GO 89999-FIM-PROCESSA
           END-IF
           IF W-ALUNO = SPACES
              DISPLAY "Campo obrigatorio"
                                       
              STOP " "
              DISPLAY W-BRANCOS        
              GO 20000-PROCESSA
           END-IF
           .
       20100-ENTRA-NOTA-1.
           DISPLAY "Informe a primeira nota do aluno"
                                       
           ACCEPT W-NOTA1              
           DISPLAY W-BRANCOS           
           IF KEY-ESC
              GO 20000-PROCESSA
           END-IF
           
           ADD W-NOTA1                 TO W-TOTAL-NOTAS
           DISPLAY W-TOTAL-NOTAS       
           .
       20200-ENTRA-NOTA-2.
           DISPLAY "Informe a segunda nota do aluno"
                                       
           ACCEPT W-NOTA2              
           DISPLAY W-BRANCOS           
           
           ADD W-NOTA2                 TO W-TOTAL-NOTAS
           DISPLAY W-TOTAL-NOTAS       
           .
       20300-ENTRA-NOTA-3.
           DISPLAY "Informe a terceira nota do aluno"
                                       
           ACCEPT W-NOTA3              
           DISPLAY W-BRANCOS           
           
           ADD W-NOTA3                 TO W-TOTAL-NOTAS
           DISPLAY W-TOTAL-NOTAS       
           .
       20400-ENTRA-NOTA-4.
           DISPLAY "Informe a primeira nota do aluno"
                                       
           ACCEPT W-NOTA4              
           DISPLAY W-BRANCOS           
           
           ADD W-NOTA4                 TO W-TOTAL-NOTAS
           DISPLAY W-TOTAL-NOTAS       
           .
       20500-CALCULA-MEDIA.
           DIVIDE W-TOTAL-NOTAS        BY 4
                                   GIVING W-MEDIA
           DISPLAY W-MEDIA             
           
           IF W-MEDIA < 5,0
              DISPLAY "REPROVADO!!!"   
           ELSE
              IF W-MEDIA < 7,0
                 DISPLAY "RECUPERACAO!!!"
                                       
              ELSE
                 DISPLAY "APROVADO!!!"
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