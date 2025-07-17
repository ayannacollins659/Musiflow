# 🎵 Musiflow - Royalty Streaming Smart Contract

## 🎯 Overview

Musiflow is a Clarity smart contract that enables **automated and transparent royalty distribution** for musicians and their collaborators. Built on the Stacks blockchain, it provides a trustless way to manage music revenue streams with real-time payouts.

## ✨ Features

- 🎼 **Create Music Streams** - Artists can register their tracks with collaborator splits
- 💰 **Automated Revenue Distribution** - Transparent percentage-based royalty splits  
- 🔄 **Real-time Withdrawals** - Collaborators can withdraw their earned royalties anytime
- 📊 **Revenue Tracking** - Complete transparency of deposits and withdrawals
- 🛡️ **Platform Fee Management** - Configurable platform fees for sustainability
- ⏸️ **Stream Management** - Artists can activate/deactivate revenue streams

## 🚀 Quick Start

### Prerequisites
- Clarinet CLI installed
- Stacks wallet for testing

### Installation

```bash
clarinet new musiflow-project
cd musiflow-project
```

Copy the contract code into `contracts/Musiflow.clar`

### Testing

```bash
clarinet console
```

## 📖 Usage Guide

### 1. Creating a Music Stream

```clarity
(contract-call? .Musiflow create-stream 
  "My Awesome Song" 
  (list 'SP1234... 'SP5678...) 
  (list u5000 u3000))
```

- **title**: Song/album title (max 100 characters)
- **collaborators**: List of principal addresses
- **percentages**: Corresponding percentage splits (in basis points, 10000 = 100%)

### 2. Depositing Revenue

```clarity
(contract-call? .Musiflow deposit-revenue u1 u1000000)
```

- **stream-id**: The ID of the music stream
- **amount**: Revenue amount in microSTX

### 3. Withdrawing Royalties

```clarity
(contract-call? .Musiflow withdraw-royalties u1)
```

Collaborators can withdraw their earned portion anytime.

### 4. Checking Withdrawable Amount

```clarity
(contract-call? .Musiflow get-withdrawable-amount u1 'SP1234...)
```

## 🔍 Read-Only Functions

| Function | Description |
|----------|-------------|
| `get-stream` | Get stream details by ID |
| `get-collaborator-percentage` | Get collaborator's percentage for a stream |
| `get-withdrawable-amount` | Check available royalties for withdrawal |
| `get-stream-collaborators` | List all collaborators for a stream |
| `get-artist-streams` | Get all streams created by an artist |
| `get-platform-fee` | Current platform fee percentage |

## 💡 Example Workflow

1. **Artist creates stream**: `create-stream` with 70% for artist, 30% for producer
2. **Revenue comes in**: Streaming platforms `deposit-revenue` 
3. **Automatic splits**: Contract calculates each party's share
4. **Instant withdrawals**: Both parties can `withdraw-royalties` anytime
5. **Full transparency**: All transactions visible on blockchain

## 🛠️ Error Codes

- `u100` - Unauthorized access
- `u101` - Stream not found  
- `u102` - Invalid amount
- `u103` - Insufficient balance
- `u104` - Already exists
- `u105` - Invalid percentage
- `u106` - Stream inactive

## 🎨 Advanced Features

### Platform Fee Management
Contract owner can adjust platform fees (max 10%):

```clarity
(contract-call? .Musiflow update-platform-fee u250)
```

### Stream Status Control
Artists can pause/resume revenue streams:

```clarity
(contract-call? .Musiflow toggle-stream-status u1)
```

## 🔐 Security Features

- ✅ Owner-only administrative functions
- ✅ Percentage validation (must sum to ≤100%)
- ✅ Balance checks before withdrawals
- ✅ Stream status validation
- ✅ Principal-based access control

## 🤝 Contributing

Feel free to submit issues and enhancement requests!

## 📄 License

MIT License - Build the future of music royalties! 

