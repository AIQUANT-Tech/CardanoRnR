import crypto from "crypto";
import fs from "fs";

function generateEncryptionKey() {
  // Generate 32 bytes (256 bits) for AES-256
  const key = crypto.randomBytes(32).toString("base64");
  return key;
}

// Generate and display the key
const encryptionKey = generateEncryptionKey();
console.log("🔑 Generated Encryption Key:");
console.log(encryptionKey);
console.log("\n📋 Add this to your .env file:");
console.log(`ENCRYPTION_KEY=${encryptionKey}`);
