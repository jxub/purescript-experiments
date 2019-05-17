module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, nubBy)
import Data.Maybe (Maybe)

type Entry =
    { firstName :: String
    , lastName  :: String
    , address   :: Address
    }

type Address =
    { street :: String
    , city   :: String
    , state  :: String
    }

type AddressBook = List Entry

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <>
                  entry.firstName <> ", " <>
                  showAddress entry.address

showAddress :: Address -> String
showAddress address = address.street <> ", " <>
                      address.city <> ", " <>
                      address.state

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = filter filterEntry >>> head
    where
        filterEntry :: Entry -> Boolean
        filterEntry entry = entry.firstName == firstName &&
            entry.lastName == lastName

removeDups :: AddressBook -> AddressBook
removeDups book = nubBy checkDups book
    where
        checkDups :: Entry -> Entry -> Boolean
        checkDups entry1 entry2 = entry1.firstName == entry2.firstName &&
            entry1.lastName == entry2.lastName

-- address = { street: "", city: "", state: ""}
-- entry = { firstName: "Jakub", lastName: "Janarek", address: address}

-- insertEntry entry book1