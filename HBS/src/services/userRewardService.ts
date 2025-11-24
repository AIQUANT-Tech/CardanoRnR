import axios from "axios";

const API_BASE_URL = import.meta.env.VITE_API_BASE_URL;

export const fetchEndUsersInfo = async () => {
  try {
    const payload = {
      loyalty_end_users_info_rq: {
        header: {
          user_name: "businessUser",
          product: "LRS",
          request_type: "FETCH_END_USER_INFO",
        },
      },
    };

    const response = await axios.post(
      `${API_BASE_URL}/user/fetchEndUsersInfo`,
      payload,
      {
        headers: { "Content-Type": "application/json" },
      }
    );

    const users =
      response?.data?.loyalty_end_users_info_rs?.user_info_list?.overall_info
        ?.user_info || [];

    return users;
  } catch (error) {
    console.error("Error fetching end users info:", error);
    throw error;
  }
};
