data Contato = Contato {nome :: String, tel :: Integer, end :: String, rel :: String} 

type Agenda = [Contato]

contatoToString :: Contato -> String
contatoToString x = "nome: "++nome x++" Telefone: "++show(tel x)++" End: "++end x++" Relacao: "++rel x

agendaToString :: Agenda -> String
agendaToString [] = " "
agendaToString x = contatoToString (head x)++" ||| "++agendaToString (tail x)

inserir :: Agenda -> Contato -> Agenda
inserir [] cont = [cont]
inserir (x:xs) cont
  |(nome x == nome cont) = cont : xs
  |otherwise = x : inserir xs cont
  
remover :: Agenda -> String -> Agenda
remover [] name = []
remover (x:xs) name
  |(nome x == name) = xs
  |otherwise = x : remover xs name
  
busca :: Agenda -> String -> Contato
busca [] name = Contato "Contato nao encontrado" 0 "" ""
busca (x:xs) name
  |(nome x == name) = x
  |otherwise = busca xs name

main :: IO ()
main =  do
let agenda = []
let agenda1 = inserir agenda (Contato "Fulano" 999999999 "Rua A" "UFF")
print("-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-")
print(agendaToString agenda1) 
let agenda2 = inserir agenda1 (Contato "Ciclano" 888888888 "Rua B" "Cederj")
print("-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-")
print(agendaToString agenda2) 
let agenda3 = inserir agenda2 (Contato "Beltrano" 88889999 "Rua C" "Infancia")
print("-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-")
print(agendaToString agenda3) 
print("-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-")
let agenda4 = inserir agenda3 (Contato "Fulano" 77777777 "Rua D" "UFF")
let agenda5 = remover agenda4 "Ciclano"
print(agendaToString agenda5)
print("-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-")
let cont1 = busca agenda3 "Beltrano"
print(contatoToString cont1)
print("-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-")
