       IDENTIFICATION DIVISION.
       PROGRAM-ID. Nomina.
       AUTHOR. MARCOS CANUL.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NOMBRE-EMPLEADO     PIC X(50).
       01 HORAS-TRABAJADAS    PIC 9(3)V99.
       01 SALARIO-POR-HORA    PIC 9(4)V99.
       01 DEDUCCIONES         PIC 9(3)V99.
       01 SALARIO-BRUTO       PIC 9(3)V99.
       01 SALARIO-NETO        PIC 9(3)V99.

       PROCEDURE DIVISION.
       INGRESO-DE-DATOS-DE-EMPLEADO.
           DISPLAY "Ingrese el nombre del empleado".
           ACCEPT NOMBRE-EMPLEADO.
           DISPLAY "Ingrese el salario/sueldo por hora".
           ACCEPT SALARIO-POR-HORA.
           DISPLAY"Ingrese las horas trabajadas por semana".
           ACCEPT HORAS-TRABAJADAS.
           

       RESULTADOS.
           DISPLAY NOMBRE-EMPLEADO.
           STOP RUN.
       END PROGRAM Nomina.
