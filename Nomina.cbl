       IDENTIFICATION DIVISION.
       PROGRAM-ID. Nomina.
       AUTHOR. MARCOS CANUL.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NOMBRE-EMPLEADO     PIC X(50).
       01 HORAS-TRABAJADAS    PIC 9(3)V99.
       01 SALARIO-POR-HORA    PIC 9(4)V99.
       01 DEDUCCIONES         PIC 9(6)V99.
       01 SALARIO-BRUTO       PIC 9(7)V99.
       01 SALARIO-NETO        PIC 9(7)V99.
       01 NUMERO-DE-EMPLEADOS PIC 9(3).

       PROCEDURE DIVISION.
       INGRESO-DE-DATOS-DE-EMPLEADO.
           PERFORM VALIDAR-NOMBRE.
           PERFORM VALIDAR-SALARIO-POR-HORA.
           PERFORM VALIDAR-HORAS-TRABAJADAS.
           PERFORM RESULTADOS.
           STOP RUN.

       VALIDAR-NOMBRE.
           DISPLAY "Ingrese el nombre del empleado".
           DISPLAY "(máximo 50 caracteres):"
           ACCEPT NOMBRE-EMPLEADO.
           IF NOMBRE-EMPLEADO = SPACES
               DISPLAY "Error: El nombre no puede estar vacío."
               PERFORM VALIDAR-NOMBRE.

       VALIDAR-SALARIO-POR-HORA.
           DISPLAY "Ingrese el salario por hora (ejemplo: 15.75):".
           ACCEPT SALARIO-POR-HORA.
           IF SALARIO-POR-HORA <= 0
               DISPLAY "Error: El salario por"
               DISPLAY"hora debe ser mayor que cero."
               PERFORM VALIDAR-SALARIO-POR-HORA.

       VALIDAR-HORAS-TRABAJADAS.
           DISPLAY "Ingrese las horas trabajadas por semana" 
           DISPLAY"(ejemplo: 40.5):"
           ACCEPT HORAS-TRABAJADAS.
           IF HORAS-TRABAJADAS < 0 OR HORAS-TRABAJADAS > 168
               DISPLAY "Error: Las horas trabajadas deben estar"
               DISPLAY"entre 0 y 168."
               PERFORM VALIDAR-HORAS-TRABAJADAS.

       RESULTADOS.
           DISPLAY NOMBRE-EMPLEADO
           STOP RUN.
       END PROGRAM Nomina.
