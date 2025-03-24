       IDENTIFICATION DIVISION.
       PROGRAM-ID. Nomina.
       AUTHOR. MARCOS CANUL.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCHIVO-NOMINA ASSIGN TO "nomina.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
           
       DATA DIVISION.
       FILE SECTION.
       FD ARCHIVO-NOMINA.
       01 REGISTRO-NOMINA.
           05 RN-NOMBRE-EMPLEADO     PIC X(50).
           05 RN-HORAS-TRABAJADAS    PIC 9(3)V99.
           05 RN-SALARIO-POR-HORA    PIC 9(4)V99.
           05 RN-SALARIO-BRUTO       PIC 9(7)V99.
           05 RN-DEDUCCIONES         PIC 9(6)V99.
           05 RN-SALARIO-NETO        PIC 9(7)V99.
       WORKING-STORAGE SECTION.
       01 NOMBRE-EMPLEADO     PIC X(50).
       01 HORAS-TRABAJADAS    PIC 9(3)V99.
       01 SALARIO-POR-HORA    PIC 9(4)V99.
       01 DEDUCCIONES         PIC 9(6)V99.
       01 SALARIO-BRUTO       PIC 9(7)V99.
       01 SALARIO-NETO        PIC 9(7)V99.
       01 NUMERO-DE-EMPLEADOS PIC 9(3).
       01 OPCION              PIC 9.
       01 OPCION-REPORTE      PIC 9.


       PROCEDURE DIVISION.
       INICIO.
           OPEN OUTPUT ARCHIVO-NOMINA.
           PERFORM MOSTRAR-MENU UNTIL OPCION = 4.
           STOP RUN.

       MOSTRAR-MENU.
           DISPLAY "======NOMINA COBOL PRACTICA======".
           DISPLAY "1. Calcular nomina de un empleado".
           DISPLAY "2. Calcular nomina de varios empleados".
           DISPLAY "3. Ver nomina de empleado".
           DISPLAY "4. Salir".
           DISPLAY "Ingrese su opción (1-4): ".
           ACCEPT OPCION.
           EVALUATE OPCION
               WHEN 1
                   PERFORM INGRESO-DE-DATOS-DE-EMPLEADO
               WHEN 2
                   DISPLAY "Funcionalidad no disponible aún."
               WHEN 3
                   DISPLAY "Funcionalidad no disponible aún."
               WHEN 4
                   PERFORM SALIR
               WHEN OTHER
                   DISPLAY "Opción inválida. Intente nuevamente."
           END-EVALUATE.


       INGRESO-DE-DATOS-DE-EMPLEADO.
           PERFORM VALIDAR-NOMBRE.
           PERFORM VALIDAR-SALARIO-POR-HORA.
           PERFORM VALIDAR-HORAS-TRABAJADAS.
           PERFORM CALCULAR-SALARIO-BRUTO.
           PERFORM CALCULAR-DEDUCCIONES.
           PERFORM CALCULAR-SALARIO-NETO.
           PERFORM GENERAR-REPORTE.
           PERFORM MENU-REPORTE-FINAL.

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
       
       MENU-REPORTE-FINAL.
           DISPLAY "==========OPERACION FINALIZADA=========="
           DISPLAY "1. REGRESAR AL MENU PRINCIPAL"
           DISPLAY "2. SALIR"
           DISPLAY "Ingrese su opción (1-2):"
           
           ACCEPT OPCION-REPORTE.
           EVALUATE OPCION-REPORTE
               WHEN 1
                   PERFORM MOSTRAR-MENU
               WHEN 2
                   PERFORM SALIR
               WHEN OTHER
                   DISPLAY "Opción inválida. Intente nuevamente."
           END-EVALUATE.


       GENERAR-REPORTE.
           DISPLAY "----------------------------------------".
           DISPLAY "|          REPORTE DE NÓMINA           |".
           DISPLAY "----------------------------------------".
           DISPLAY "|Nombre del empleado:|", NOMBRE-EMPLEADO(1:25).
           DISPLAY "|Horas trabajadas:   |", HORAS-TRABAJADAS.
           DISPLAY "|Salario por hora:   |", SALARIO-POR-HORA.
           DISPLAY "|Salario bruto:      |", SALARIO-BRUTO.
           DISPLAY "|Deducciones:        |", DEDUCCIONES.
           DISPLAY "|Salario neto:       |", SALARIO-NETO.
           DISPLAY "----------------------------------------"
           PERFORM GENERAR-ARCHIVO.

       GENERAR-ARCHIVO.
           MOVE NOMBRE-EMPLEADO TO RN-NOMBRE-EMPLEADO.
           MOVE HORAS-TRABAJADAS TO RN-HORAS-TRABAJADAS.
           MOVE SALARIO-POR-HORA TO RN-SALARIO-POR-HORA.
           MOVE SALARIO-BRUTO TO RN-SALARIO-BRUTO.
           MOVE DEDUCCIONES TO RN-DEDUCCIONES.
           MOVE SALARIO-NETO TO RN-SALARIO-NETO.
           WRITE REGISTRO-NOMINA.
           DISPLAY "Datos guardados en el archivo 'nomina.txt'".

       SALIR.
           CLOSE ARCHIVO-NOMINA.
           DISPLAY "========SALIENDO DEL PROGRAMA....=======".
           STOP RUN.
       END PROGRAM Nomina.
