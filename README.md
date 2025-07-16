# 🤖 Humanchain - Proof of Humanity Smart Contract

A Stacks blockchain implementation for Sybil-resistant human verification using social vouching.

## 🎯 Features

- 👤 Human registration with STX deposit
- ✅ Social vouching system
- 🔒 Sybil resistance through multi-party verification
- ⏰ Cooling period for revoked registrations
- 📊 Registration status tracking

## 🚀 Contract Functions

### Public Functions

1. `register()`
   - Register as a human
   - Requires 100 STX deposit
   - Cannot register if already registered

2. `vouch-for(human)`
   - Vouch for another registered human
   - Each human can only vouch once for another human
   - Cannot vouch for yourself

3. `revoke-registration()`
   - Revoke your own registration
   - Initiates cooling period before re-registration

### Read-Only Functions

1. `is-registered(human)`
   - Check if an address is registered

2. `get-human-details(human)`
   - Get detailed information about a registration

3. `get-total-humans()`
   - Get total number of registered humans

4. `can-register(human)`
   - Check if an address is eligible to register

5. `has-vouched(voucher, human)`
   - Check if one address has vouched for another

## 💡 Usage

1. Deploy the contract to the Stacks blockchain
2. Register by calling `register()` with required STX
3. Get other registered humans to vouch for you
4. Maintain registration or revoke if needed

## ⚙️ Constants

- Registration Cost: 100 STX
- Required Vouches: 3
- Cooling Period: 144 blocks

## 🔐 Security

- Multi-party verification system
- Cooling period prevents quick re-registrations
- STX deposit requirement
- One-way vouching mechanism
```


