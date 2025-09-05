# Carbon Credit Trading Marketplace
A decentralized marketplace for trading verified carbon credits on the Stacks blockchain.

## 🚀 Features

- Mint new carbon credits as NFTs
- List/unlist credits for sale
- Buy carbon credits
- Transfer credits between accounts
- Verifier management system
- Immutable project data storage

## 📋 Contract Functions

### For Contract Owner
- `register-verifier`: Add new authorized verifiers
- `remove-verifier`: Remove verifier access

### For Verifiers
- `mint-carbon-credits`: Create new carbon credit NFTs with project data

### For Users
- `list-credits`: List credits for sale
- `unlist-credits`: Remove credits from marketplace
- `buy-credits`: Purchase listed credits
- `transfer-credits`: Transfer credits to another address
- `get-credit-data`: View credit details
- `get-credit-owner`: Check credit ownership

## 🔧 Usage

1. Deploy contract using Clarinet
2. Register verifiers through contract owner
3. Verifiers can mint new carbon credits
4. Users can trade credits on the marketplace

## 💡 Example

```clarity
;; Mint new credits as verifier
(contract-call? .carbon-credit-marketplace mint-carbon-credits 
    "Amazon Rainforest Protection" 
    "Brazil" 
    u2023 
    u1000 
    "VCS" 
    u100)

;; List credits for sale
(contract-call? .carbon-credit-marketplace list-credits u1 u150)

;; Buy listed credits
(contract-call? .carbon-credit-marketplace buy-credits u1)
```
```

