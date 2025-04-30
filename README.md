# BrightLink

A decentralized professional networking application built on Stacks, offering secure contact management and follow-up tracking using Clarity smart contracts.

BrightLink aims to create a decentralized alternative to professional networking platforms by leveraging blockchain technology for data ownership and privacy. The platform enables users to establish their professional identity, build a network of contacts with granular privacy controls, and manage professional interactions with follow-up reminders and task tracking.

## Features

- Professional identity management with privacy controls
- Connection management with customizable relationship types
- Follow-up task tracking and reminders
- Skill endorsements and professional reputation system
- Professional achievements and credentials verification

## Smart Contracts

The platform consists of four main smart contracts:

### brightlink-identity

Manages user profiles and professional identity information:
- Create and update professional profiles
- Control privacy settings for profile fields
- Manage professional skills and experiences
- Track skill endorsements
- Store professional history with privacy controls

Key functions:
- `create-or-update-profile`: Create or update user profile information
- `set-privacy-setting`: Control visibility of profile fields
- `add-skill`: Add professional skills to profile
- `add-experience`: Add professional experience entries
- `update-experience`: Modify existing experience entries

### brightlink-connections

Handles professional connections between users:
- Send and receive connection requests
- Manage connection relationships and types
- Tag and categorize connections
- Control connection visibility
- Archive inactive connections

Key functions:
- `send-connection-request`: Initiate a connection with another user
- `accept-connection-request`: Accept incoming connection requests
- `update-relationship-type`: Modify connection relationship classification
- `add-tag`: Add organizational tags to connections
- `archive-connection`: Move connections to archived status

### brightlink-followups

Manages professional follow-up tasks and reminders:
- Create follow-up tasks with deadlines
- Track recurring follow-ups
- Manage task status and completion
- Organize follow-ups by contact

Key functions:
- `create-followup`: Create new follow-up tasks
- `update-followup`: Modify existing follow-ups
- `complete-followup`: Mark tasks as completed
- `cancel-followup`: Cancel scheduled follow-ups

### brightlink-reputation

Implements a decentralized professional reputation system:
- Receive verifiable skill endorsements
- Collect professional testimonials
- Verify professional achievements
- Track credential issuance and validation

Key functions:
- `endorse-skill`: Endorse users for specific skills
- `create-testimonial`: Submit professional testimonials
- `issue-achievement`: Issue verified professional achievements
- `verify-achievement`: Validate claimed achievements

## Data Privacy

BrightLink implements multiple levels of privacy controls:
- Public: Visible to all users
- Connections-only: Visible only to connected users
- Private: Visible only to the profile owner

Users have granular control over the visibility of:
- Profile information
- Professional experiences
- Connection relationships
- Skills and endorsements

## Getting Started

This project is built with Clarity smart contracts for the Stacks blockchain. To interact with the contracts:

1. Deploy the contracts to the Stacks blockchain
2. Initialize user profile using `brightlink-identity` contract
3. Build connections network using `brightlink-connections` contract
4. Manage follow-ups using `brightlink-followups` contract
5. Build reputation through endorsements and testimonials using `brightlink-reputation` contract

## Development

The contracts can be tested using the Clarity REPL or through integration with a dApp frontend. Ensure proper error handling and privacy controls when building interfaces to these contracts.

## Security Considerations

- Users should carefully manage privacy settings for sensitive professional information
- Connection requests should be verified before acceptance
- Achievement and credential issuers should be validated
- Follow-up task information is private to the creating user