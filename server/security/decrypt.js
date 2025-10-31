import crypto from "crypto";
import dotenv from "dotenv";

// Load environment variables
dotenv.config();

const algorithm = "aes-256-gcm";

function decrypt(encrypted, keyBase64) {
  const key = Buffer.from(keyBase64, "base64");
  const parts = encrypted.split(":");

  if (parts.length !== 3) {
    throw new Error(
      "Invalid encrypted format. Expected format: iv:authTag:encryptedData"
    );
  }

  const iv = Buffer.from(parts[0], "hex");
  const authTag = Buffer.from(parts[1], "hex");
  const encryptedText = parts[2];

  const decipher = crypto.createDecipheriv(algorithm, key, iv);
  decipher.setAuthTag(authTag);

  let decrypted = decipher.update(encryptedText, "hex", "utf8");
  decrypted += decipher.final("utf8");
  return decrypted;
}

// Get values from environment
const encryptedString = process.env.DB_CNN;
const encryptionKey = process.env.ENCRYPTION_KEY;

if (!encryptedString || !encryptionKey) {
  console.error("❌ Missing environment variables:");
  if (!encryptedString) console.error("   - DB_CNN not found");
  if (!encryptionKey) console.error("   - ENCRYPTION_KEY not found");
  process.exit(1);
}

try {
  const decrypted = decrypt(encryptedString, encryptionKey);
  console.log("✅ Decryption successful!");
  console.log("");
  console.log("🔓 Decrypted MongoDB URI:");
  console.log(decrypted);

  // Show masked version for security
  const maskedURI = decrypted.replace(/:([^:]+)@/, ":****@");
  console.log("");
  console.log("🎭 Masked URI (for verification):");
  console.log(maskedURI);
} catch (error) {
  console.error("❌ Decryption failed:", error.message);
  console.log("");
  console.log("💡 Possible issues:");
  console.log("   - Invalid encryption key");
  console.log("   - Corrupted encrypted data");
  console.log("   - Wrong encrypted string format");
  process.exit(1);
}
