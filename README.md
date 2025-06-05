# 🏠 Leasepad - On-Chain Lease Agreements

> 📝 Digitally signed and verifiable rental contracts on the Stacks blockchain

## 🌟 Overview

Leasepad is a smart contract platform that enables landlords and tenants to create, sign, and manage rental agreements entirely on-chain. All lease terms, signatures, and payments are recorded immutably on the blockchain, providing transparency and trust for both parties.

## ✨ Features

- 🔐 **Digital Signatures**: Both landlord and tenant must sign the lease
- 💰 **Rent Payments**: Track monthly rent payments on-chain
- 🛡️ **Security Deposits**: Handle security deposit transfers
- ⏰ **Lease Duration**: Automatic expiration based on block height
- 📊 **Payment History**: Complete audit trail of all transactions
- 🔍 **Lease Status**: Real-time lease status tracking

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet for testing

### Installation

```bash
git clone <your-repo>
cd leasepad
clarinet check
```

## 📖 Usage

### Creating a Lease

Landlords can create a new lease agreement:

```clarity
(contract-call? .Leasepad create-lease 
  'ST1TENANT-ADDRESS
  "123 Main St, Apt 4B"
  u1000000  ;; 1 STX monthly rent
  u2000000  ;; 2 STX security deposit
  u52560)   ;; ~1 year in blocks
```

### Signing a Lease

Both parties must sign the lease to activate it:

```clarity
(contract-call? .Leasepad sign-lease u1)
```

### Paying Rent

Tenants can pay monthly rent:

```clarity
(contract-call? .Leasepad pay-rent u1 u1) ;; lease-id, month
```

### Paying Security Deposit

```clarity
(contract-call? .Leasepad pay-security-deposit u1)
```

## 🔍 Read-Only Functions

### Get Lease Details
```clarity
(contract-call? .Leasepad get-lease u1)
```

### Check Payment History
```clarity
(contract-call? .Leasepad get-payment u1 u1) ;; lease-id, month
```

### Get User's Leases
```clarity
(contract-call? .Leasepad get-user-leases 'ST1USER-ADDRESS)
```

### Check Lease Status
```clarity
(contract-call? .Leasepad get-lease-status u1)
(contract-call? .Leasepad is-lease-active u1)
(contract-call? .Leasepad is-lease-expired u1)
```

## 📋 Lease Lifecycle

1. **Creation** 🏗️ - Landlord creates lease (auto-signed by landlord)
2. **Pending** ⏳ - Waiting for tenant signature
3. **Active** ✅ - Both parties signed, lease is active
4. **Terminated** ❌ - Lease terminated by either party
5. **Expired** ⏰ - Lease duration has ended

## 🛠️ Development

### Testing

```bash
clarinet test
```

### Console

```bash
clarinet console
```

## 🔒 Security Features

- ✅ Authorization checks for all operations
- ✅ Duplicate payment prevention
- ✅ Lease expiration validation
- ✅ Status-based operation restrictions
- ✅ Principal verification for signatures

## 📊 Error Codes

| Code | Description |
|------|-------------|
| u100 | Not authorized |
| u101 | Lease not found |
| u102 | Lease already exists |
| u103 | Lease expired |
| u104 | Lease not active |
| u105 | Already signed |
| u106 | Invalid amount |
| u107 | Payment failed |
| u108 | Lease terminated |

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch
3. Commit your changes
4. Push to the branch
5. Create a Pull Request

## 📄 License

This project is licensed under the MIT License.


