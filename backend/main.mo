import Principal "mo:base/Principal";
import Buffer "mo:base/Buffer";
import HashMap "mo:base/HashMap";
import Text "mo:base/Text";
import Nat "mo:base/Nat";
import Debug "mo:base/Debug";
import Iter "mo:base/Iter";
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
    let curpToId = HashMap.HashMap<Text, Text>(10, Text.equal, Text.hash);
    let idToCurp = HashMap.HashMap<Text, Text>(10, Text.equal, Text.hash);
    let documentos = HashMap.HashMap<Text, [Documento]>(10, Text.equal, Text.hash);
    private func generateKey() : Text {
        patientKey += 1;
        Nat.toText(patientKey);
    };
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
      Debug.print("IDPaciente registrado con CURP: " # curp);
      id
  };
  public query func getPatientByCURP(curp : Text) : async ?Patient {
      switch(curpToId.get(curp)) {
          case (?id) {
              Array.find<Patient>(
                  Buffer.toArray(patients),
                  func(p : Patient) = p.id == id
              )
          };
          case null { null };
      };
  };  
    public shared(msg) func subirDocumentos(
        curp : Text,
        enlace : Text
    ) : async Text {
        switch(curpToId.get(curp)) {
            case (?id) {
                let buffer = Buffer.Buffer<Documento>(1);
                switch(documentos.get(curp)) {
                    case (?docs) {
                      for (doc in docs.vals()) {
                          buffer.add(doc);
                      };
                  };
                    case null {};
                };
                buffer.add({
                    idPaciente = id;
                    curp = curp;
                    enlace = enlace;
                });
                documentos.put(curp, Buffer.toArray(buffer));
                "Documento agregado correctamente"
            };
            case null {
                "Error: CURP no registrado en el sistema"
            };
          };
        };
  public query func obtenerInfoSegura(id : Text) : async ?{
    edad : Nat;
    tipoSangre : Text;
    documentos : [Text];
} {
    switch(idToCurp.get(id)) {
        case (?curp) {
            let pacienteEncontrado = Array.find<Patient>(
                Buffer.toArray(patients),
                func(p : Patient) = p.id == id
            );
            switch(pacienteEncontrado) {
                case (?patient) {
                    let docs = switch(documentos.get(curp)) {
                        case (?d) { d };
                        case null { [] };
                    };
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
              var index : Nat = 0;
              var found = false;
              label l for (p in patients.vals()) {
                  if (p.id == id) {
                      found := true;
                      break l;
                  };
                  index += 1;
              };
              if (found) {
                  let originalPatient = patients.get(index);
                  let updatedPatient : Patient = {
                      id = id;
                      nombre = nombre;
                      edad = edad;
                      nacimiento = nacimiento;
                      sexo = sexo;
                      tipoSangre = tipoSangre;
                      curp = originalPatient.curp; 
                  };
                  patients.put(index, updatedPatient);
                  "Paciente actualizado"
              } else {
                  "Error: ID no encontrado"
              }
          };
          case null { "Error: CURP inválido" };
      };
  };
  public shared(msg) func deletePatient(curp : Text) : async Text {
      switch(curpToId.get(curp)) {
          case (?id) {
              var index : Nat = 0;
              var found = false;
              label l for (p in patients.vals()) {
                  if (p.id == id) {
                      found := true;
                      break l;
                  };
                  index += 1;
              };
              
              if (found) {
                  ignore patients.remove(index);
                  ignore curpToId.remove(curp);
                  ignore idToCurp.remove(id);
                  "Paciente eliminado"
              } else {
                  "Error: ID no encontrado"
              }
          };
          case null { "Error: CURP inválido" };
      };
  };
};
