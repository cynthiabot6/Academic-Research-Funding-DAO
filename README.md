# Academic Research Funding DAO

A decentralized autonomous organization (DAO) built on Stacks blockchain for democratizing academic research funding.

## Overview

This DAO enables researchers to submit funding proposals that can be voted on by community members. It provides a transparent and decentralized way to allocate research funds based on community consensus.

## Features

- Submit research proposals with title and funding amount
- Community voting mechanism with one vote per address
- Proposal tracking and vote counting
- Built-in safeguards against double voting
- Transparent on-chain proposal data

## Smart Contract Functions

### Public Functions

`submit-proposal`
- Submit a new research funding proposal
- Parameters:
  - title: String (max 50 chars)
  - funding-amount: Integer
- Returns proposal ID

`vote`
- Vote on an existing proposal
- Parameters:
  - proposal-id: Integer
- Returns success/failure

### Read-Only Functions

`get-proposal`
- Get details of a specific proposal
- Parameters:
  - proposal-id: Integer
- Returns proposal data

`get-proposal-count`
- Get total number of proposals
- Returns proposal count

## Testing

The project includes comprehensive tests covering:
- Proposal submission
- Voting mechanics
- Error handling
- Data validation

