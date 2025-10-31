import crypto from "crypto";
import dotenv from "dotenv";

// Load environment variables
dotenv.config();

const algorithm = "aes-256-gcm";

function validateKey(keyBase64) {
  try {
    const key = Buffer.from(keyBase64, "base64");
    return key.length === 32;
  } catch {
    return false;
  }
}

function encrypt(text, keyBase64) {
  if (!validateKey(keyBase64)) {
    throw new Error("Invalid encryption key. Must be 32 bytes base64 encoded.");
  }

  const key = Buffer.from(keyBase64, "base64");
  const iv = crypto.randomBytes(16);

  const cipher = crypto.createCipheriv(algorithm, key, iv);
  let encrypted = cipher.update(text, "utf8", "hex");
  encrypted += cipher.final("hex");
  const authTag = cipher.getAuthTag();

  return `${iv.toString("hex")}:${authTag.toString("hex")}:${encrypted}`;
}

// ✅ Get Mongo URI from command line arguments
const mongoURI = process.argv[2]; // The 3rd argument (index 2)
const encryptionKey = process.env.ENCRYPTION_KEY;

if (!mongoURI) {
  console.error("❌ Please provide a Mongo URI as a command-line argument.");
  console.log("Example:");
  console.log("   node encrypt.js 'mongodb+srv://user:pass@cluster/dbname'");
  process.exit(1);
}

if (!encryptionKey) {
  console.error("❌ ENCRYPTION_KEY not found in environment variables");
  console.log(
    "Please run keygeneration.js first and add the key to your .env file"
  );
  process.exit(1);
}

try {
  const encrypted = encrypt(mongoURI, encryptionKey);
  console.log("✅ Encryption successful!\n");
  console.log("📝 Original URI:", mongoURI, "\n");
  console.log("🔑 Add this to your .env file:");
  console.log(`DB_CNN=${encrypted}\n`);
  console.log("📋 Format: iv:authTag:encryptedData");
} catch (error) {
  console.error("❌ Encryption failed:", error.message);
  process.exit(1);
}
