import Principal "mo:base/Principal";
import Buffer "mo:base/Buffer";
import HashMap "mo:base/HashMap";
import Text "mo:base/Text";
import Nat "mo:base/Nat";
import Debug "mo:base/Debug";
import Array "mo:base/Array";

actor {
    type Patient = {
        id : Text;
        nombre : Text;
        edad : Nat;
        nacimiento : Text;
        sexo : Text;
        tipoSangre : Text;
        curp : Text;
    };

    type Documento = {
        idPaciente : Text;
        curp : Text;
        enlace : Text;
    };

    var patientKey = 0;
    let patients = Buffer.Buffer<Patient>(0);
    
    // Mapeos CURP <-> ID
    let curpToId = HashMap.HashMap<Text, Text>(10, Text.equal, Text.hash);
    let idToCurp = HashMap.HashMap<Text, Text>(10, Text.equal, Text.hash);
    
    // Almacenamiento de documentos por CURP
    let documentos = HashMap.HashMap<Text, [Documento]>(10, Text.equal, Text.hash);

    private func generateKey() : Text {
        patientKey += 1;
        Nat.toText(patientKey);
    };

    // ========== FUNCIONES PRINCIPALES ==========
    
    public shared(msg) func createPatient(
        nombre : Text,
        edad : Nat,
        nacimiento : Text,
        sexo : Text,
        tipoSangre : Text,
        curp : Text
    ) : async Text {
        let id = generateKey();
        let newPatient : Patient = {
            id = id;
            nombre = nombre;
            edad = edad;
            nacimiento = nacimiento;
            sexo = sexo;
            tipoSangre = tipoSangre;
            curp = curp;
        };
        
        patients.add(newPatient);
        curpToId.put(curp, id);
        idToCurp.put(id, curp);
        Debug.print("Paciente registrado con CURP: " # curp);
        id
    };

    public query func getPatientByCURP(curp : Text) : async ?Patient {
        switch(curpToId.get(curp)) {
            case (?id) { findPatientById(id) };
            case null { null };
        };
    };

    // ========== GESTIÓN DE DOCUMENTOS ==========
    
    public shared(msg) func subirDocumentos(
        curp : Text,
        enlace : Text
    ) : async Text {
        switch(curpToId.get(curp)) {
            case (?id) {
                let nuevosDocs = Array.append(
                    documentos.get(curp) ? [],
                    [{
                        idPaciente = id;
                        curp = curp;
                        enlace = enlace;
                    }]
                );
                documentos.put(curp, nuevosDocs);
                "Documento agregado correctamente"
            };
            case null {
                "Error: CURP no registrado en el sistema"
            };
        };
    };

    // ========== ACCESO SEGURO ==========
    
    public query func obtenerInfoSegura(id : Text) : async ?{
        edad : Nat;
        tipoSangre : Text;
        documentos : [Text];
    } {
        switch(idToCurp.get(id)) {
            case (?curp) {
                switch(findPatientById(id)) {
                    case (?patient) {
                        let docs = documentos.get(curp) ? [];
                        let enlaces = Array.map<Documento, Text>(
                            docs,
                            func(d) { d.enlace }
                        );
                        ?{
                            edad = patient.edad;
                            tipoSangre = patient.tipoSangre;
                            documentos = enlaces;
                        }
                    };
                    case null { null };
                };
            };
            case null { null };
        };
    };

    // ========== FUNCIONES ACTUALIZADAS ==========
    
    public shared(msg) func updatePatient(
        curp : Text,
        nombre : Text,
        edad : Nat,
        nacimiento : Text,
        sexo : Text,
        tipoSangre : Text
    ) : async Text {
        switch(curpToId.get(curp)) {
            case (?id) {
                switch(findPatientIndex(id)) {
                    case (?index) {
                        let original = patients.get(index);
                        let updated : Patient = {
                            original with
                            nombre;
                            edad;
                            nacimiento;
                            sexo;
                            tipoSangre;
                        };
                        patients.put(index, updated);
                        "Paciente actualizado"
                    };
                    case null { "Error: Paciente no encontrado" };
                };
            };
            case null { "Error: CURP inválido" };
        };
    };

    public shared(msg) func deletePatient(curp : Text) : async Text {
        switch(curpToId.get(curp)) {
            case (?id) {
                switch(findPatientIndex(id)) {
                    case (?index) {
                        ignore patients.remove(index);
                        ignore curpToId.remove(curp);
                        ignore idToCurp.remove(id);
                        "Paciente eliminado"
                    };
                    case null { "Error: Paciente no encontrado" };
                };
            };
            case null { "Error: CURP inválido" };
        };
    };

    // ========== FUNCIONES AUXILIARES ==========
    
    private func findPatientById(id : Text) : ?Patient {
        Array.find<Patient>(
            Buffer.toArray(patients),
            func(p : Patient) = p.id == id
        )
    };

    private func findPatientIndex(id : Text) : ?Nat {
        Buffer.indexOf<Text, Patient>(
            id,
            patients,
            func(p : Patient, id) = p.id == id
        )
    };

    public query func getAllPatients() : async [Patient] {
        Buffer.toArray(patients)
    };

    public query(msg) func whoAmI() : async Principal {
        msg.caller
    };
};