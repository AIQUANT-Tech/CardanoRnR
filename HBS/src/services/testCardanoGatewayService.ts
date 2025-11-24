interface GetKeyResponse {
  accessKey: string;
  expiresIn: number;
}

export interface CreateOrderPayload {
  accessKey: string;
  amount: number;
  currency: string;
  network: string;
}

export interface OrderResponse {
  orderId: string;
  amount: number;
  currency: string;
  network: string;
  status: string;
  merchantCid: string;
}
const baseUrl = import.meta.env.VITE_PAYMENT_API_BASE_URL;
const paymentUrl = `${baseUrl}/payment`;

export async function getMerchantKey(): Promise<GetKeyResponse> {
  const url = `${baseUrl}/merchant/get-key`;

  try {
    const response = await fetch(url, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      // ✅ You can pass merchant credentials if needed, else skip
      // body: JSON.stringify({ cid: "your-cid", csecret: "your-secret" })
    });

    if (!response.ok) {
      throw new Error(`Failed to fetch merchant key: ${response.statusText}`);
    }

    const data: GetKeyResponse = await response.json();
    return data;
  } catch (error) {
    console.error("Error fetching merchant key:", error);
    throw error;
  }
}


export async function createOrder(
  data: CreateOrderPayload
): Promise<OrderResponse> {
  try {
    const response = await fetch(`${paymentUrl}/create-order`, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify(data),
    });

    if (!response.ok) {
      const err = await response.json();
      throw new Error(err.error || "Failed to create order");
    }

    return await response.json();
  } catch (error: any) {
    console.error("❌ Error creating order:", error);
    throw error;
  }  
}

export async function completeCheckout(orderId: string, txHash: string) {
  try {
    const response = await fetch(`${paymentUrl}/complete-checkout`, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify({ orderId, txHash }),
    });

    if (!response.ok) {
      const err = await response.json();
      throw new Error(err.error || "Failed to complete checkout");
    }

    return await response.json();
  } catch (error: any) {
    console.error("❌ completeCheckout error:", error);
    throw error;
  }
}
  