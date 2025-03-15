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
           PERFORM CALCULAR-SALARIO-BRUTO.
           PERFORM CALCULAR-DEDUCCIONES.
           PERFORM CALCULAR-SALARIO-NETO.
           PERFORM GENERAR-REPORTE.
           STOP RUN.

       VALIDAR-NOMBRE.
           DISPLAY "Ingrese el nombre del empleado".
           DISPLAY "(máximo 50 caracteres):"
           ACCEPT NOMBRE-EMPLEADO.
           IF NOMBRE-EMPLEADO = SPACES
               DISPLAY "Error: El nombre no puede estar vacío."
               PERFORM VALIDAR-NOMBRE.

       VALIDAR-SALARIO-POR-HORA.
           DISPLAY "Ingrese el salario por hora en USD (Ej: 15.75):".
           ACCEPT SALARIO-POR-HORA.
           IF SALARIO-POR-HORA <= 0
               DISPLAY "==========ERROR=========="
               DISPLAY"El salario por hora debe ser mayor que 0 USD."
               PERFORM VALIDAR-SALARIO-POR-HORA.

       VALIDAR-HORAS-TRABAJADAS.
           DISPLAY "Ingrese las horas trabajadas por semana" 
           DISPLAY"(ejemplo: 40.5):"
           ACCEPT HORAS-TRABAJADAS.
           IF HORAS-TRABAJADAS < 0 OR HORAS-TRABAJADAS > 168
               DISPLAY "==========ERROR=========="
               DISPLAY"Las horas trabajadas deben estar entre 0 y 168."
               PERFORM VALIDAR-HORAS-TRABAJADAS.
       
       CALCULAR-SALARIO-BRUTO.
           COMPUTE SALARIO-BRUTO = HORAS-TRABAJADAS * SALARIO-POR-HORA.

       CALCULAR-DEDUCCIONES.
           COMPUTE DEDUCCIONES = SALARIO-BRUTO * 0.15. *>15% de impuestos.

       CALCULAR-SALARIO-NETO.
           COMPUTE SALARIO-NETO = SALARIO-BRUTO - DEDUCCIONES.

       GENERAR-REPORTE.
           DISPLAY "----------------------------------------".
           DISPLAY "Nombre del empleado: ", NOMBRE-EMPLEADO.
           DISPLAY "Horas trabajadas: ", HORAS-TRABAJADAS.
           DISPLAY "Salario por hora: ", SALARIO-POR-HORA.
           DISPLAY "Salario bruto: ", SALARIO-BRUTO.
           DISPLAY "Deducciones: ", DEDUCCIONES.
           DISPLAY "Salario neto: ", SALARIO-NETO.
           DISPLAY "----------------------------------------".
           STOP RUN.
       END PROGRAM Nomina.
