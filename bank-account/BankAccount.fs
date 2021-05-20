module BankAccount

type Msgs =
   | CreateAccount of AsyncReplyChannel<int>
   | CloseAccount of int
   | GetBalance of int * AsyncReplyChannel<decimal option>
   | UpdateBalance of int * decimal

let mb =
    MailboxProcessor.Start( fun mb ->
        let rec bank (ids, data) = async {
             match! mb.Receive() with
             | CreateAccount rc ->
                rc.Reply ids
                return! bank (ids+1, data |> Map.add ids 0m)
             | CloseAccount acc ->
                return! bank (ids, data |> Map.remove acc)
             | GetBalance (acc, rc) ->
                data |> Map.tryFind acc |> rc.Reply
                return! bank (ids, data)
             | UpdateBalance (acc, amount) ->
                match data |> Map.tryFind acc with
                | Some n -> return! bank (ids, data |> Map.add acc (n + amount))
                | None -> return! bank (ids, data)
            }
        bank (0, Map.empty)
    )

let mkBankAccount() =
    mb.PostAndReply CreateAccount

let openAccount = id

let closeAccount id =
    mb.Post (CloseAccount id)
    id

let getBalance id =
    mb.PostAndReply (fun rc -> GetBalance(id, rc))

let updateBalance amount id =
    mb.Post(UpdateBalance (id,amount))
    id