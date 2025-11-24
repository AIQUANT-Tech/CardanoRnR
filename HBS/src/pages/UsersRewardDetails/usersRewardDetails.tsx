import { useEffect, useState } from "react";
import { fetchEndUsersInfo } from "../../services/userRewardService";
import { Award, Mail, User, TrendingUp } from "lucide-react";

type User = {
  user_id: string;
  user_name: string;
  last_name: string;
  email: string;
  tier?: {
    tier_name: string;
  };
  reward_balance: number;
};

export default function UsersRewardDetails() {
  const [users, setUsers] = useState<User[]>([]);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    const loadUsers = async () => {
      try {
        const data = await fetchEndUsersInfo();
        setUsers(data);
      } catch (err) {
        console.error("Failed to load users:", err);
      } finally {
        setLoading(false);
      }
    };
    loadUsers();
  }, []);

  return (
    <div className="flex flex-col min-h-screen bg-gradient-to-br from-slate-50 via-amber-50/30 to-slate-100">
      <main className="flex-grow max-w-7xl mx-auto px-6 py-12 w-full">
        
        <div className="text-center mb-12">
          <div className="flex items-center justify-center gap-3 mb-4">
            <div className="bg-amber-500 p-3 rounded-full">
              <Award className="text-white" size={32} />
            </div>
            <h1 className="text-5xl font-extrabold bg-gradient-to-r from-amber-600 to-amber-500 bg-clip-text text-transparent">
              Atithi Rewards
            </h1>
          </div>
          <p className="text-gray-600 text-lg">
            Manage and track loyalty program members
          </p>
        </div>

        {/* Stats Cards */}
        <div className="grid grid-cols-1 md:grid-cols-3 gap-6 mb-10">
          <div className="bg-white rounded-xl shadow-lg p-6 border-l-4 border-amber-500">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-gray-500 text-sm font-medium">
                  Total Members
                </p>
                <p className="text-3xl font-bold text-slate-800 mt-1">
                  {users.length}
                </p>
              </div>
              <div className="bg-amber-100 p-3 rounded-full">
                <User className="text-amber-600" size={24} />
              </div>
            </div>
          </div>

          <div className="bg-white rounded-xl shadow-lg p-6 border-l-4 border-blue-500">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-gray-500 text-sm font-medium">
                  Active Tiers
                </p>
                <p className="text-3xl font-bold text-slate-800 mt-1">
                  {
                    new Set(users.map((u) => u.tier?.tier_name).filter(Boolean))
                      .size
                  }
                </p>
              </div>
              <div className="bg-blue-100 p-3 rounded-full">
                <TrendingUp className="text-blue-600" size={24} />
              </div>
            </div>
          </div>

          <div className="bg-white rounded-xl shadow-lg p-6 border-l-4 border-green-500">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-gray-500 text-sm font-medium">
                  Total Rewards
                </p>
                <p className="text-3xl font-bold text-slate-800 mt-1">
                  {users
                    .reduce((sum, u) => sum + u.reward_balance, 0)
                    .toFixed(5)}
                </p>
              </div>
              <div className="bg-green-100 p-3 rounded-full">
                <Award className="text-green-600" size={24} />
              </div>
            </div>
          </div>
        </div>

        
        {loading ? (
          <div className="bg-white rounded-2xl shadow-xl p-20 text-center">
            <div className="inline-block animate-spin rounded-full h-12 w-12 border-4 border-amber-500 border-t-transparent mb-4"></div>
            <p className="text-lg text-gray-600 font-medium">
              Loading members...
            </p>
          </div>
        ) : users.length === 0 ? (
          <div className="bg-white rounded-2xl shadow-xl p-20 text-center">
            <div className="bg-gray-100 w-20 h-20 rounded-full flex items-center justify-center mx-auto mb-4">
              <User className="text-gray-400" size={40} />
            </div>
            <p className="text-xl text-gray-600 font-medium">
              No members found
            </p>
            <p className="text-gray-500 mt-2">
              Start adding members to see them here
            </p>
          </div>
        ) : (
          <div className="bg-white rounded-2xl shadow-xl overflow-hidden mt-10">
        
            <div className="overflow-x-auto">
              <table className="w-full">
                <thead className="bg-slate-100 border-b-2 border-slate-200">
                  <tr>
                    <th className="py-4 px-6 text-left text-xs font-bold text-slate-700 uppercase tracking-wider">
                      Member ID
                    </th>
                    <th className="py-4 px-6 text-left text-xs font-bold text-slate-700 uppercase tracking-wider">
                      Name
                    </th>
                    <th className="py-4 px-6 text-left text-xs font-bold text-slate-700 uppercase tracking-wider">
                      Email
                    </th>
                    <th className="py-4 px-6 text-left text-xs font-bold text-slate-700 uppercase tracking-wider">
                      Tier
                    </th>
                    <th className="py-4 px-6 text-right text-xs font-bold text-slate-700 uppercase tracking-wider">
                      Reward Balance
                    </th>
                  </tr>
                </thead>
                <tbody className="divide-y divide-slate-200">
                  {users.map((user, index) => (
                    <tr
                      key={user.user_id}
                      className={`hover:bg-amber-50 transition-all duration-200 ${
                        index % 2 === 0 ? "bg-white" : "bg-slate-50"
                      }`}
                    >
                      <td className="py-4 px-6">
                        <span className="text-slate-600 font-mono text-sm">
                          #{user.user_id}
                        </span>
                      </td>
                      <td className="py-4 px-6">
                        <div className="flex items-center gap-3">
                          <div className="bg-amber-100 w-10 h-10 rounded-full flex items-center justify-center">
                            <User className="text-amber-600" size={20} />
                          </div>
                          <span className="font-semibold text-slate-800">
                            {user.user_name} {user.last_name}
                          </span>
                        </div>
                      </td>
                      <td className="py-4 px-6">
                        <div className="flex items-center gap-2 text-slate-600">
                          <Mail size={16} className="text-gray-400" />
                          <span className="text-sm">{user.email}</span>
                        </div>
                      </td>
                      <td className="py-4 px-6">
                        <span
                          className={`inline-flex items-center px-3 py-1 rounded-full text-xs font-bold ${
                            user.tier?.tier_name === "Gold"
                              ? "bg-yellow-100 text-yellow-800"
                              : user.tier?.tier_name === "Silver"
                              ? "bg-gray-100 text-gray-800"
                              : user.tier?.tier_name === "Bronze"
                              ? "bg-orange-100 text-orange-800"
                              : user.tier?.tier_name === "Platinum"
                              ? "bg-purple-100 text-purple-800"
                              : "bg-slate-100 text-slate-800"
                          }`}
                        >
                          <Award size={12} className="mr-1" />
                          {user.tier?.tier_name || "N/A"}
                        </span>
                      </td>
                      <td className="py-4 px-6 text-right">
                        <div className="inline-flex items-center gap-2 bg-green-100 px-3 py-1 rounded-lg">
                          <span className="text-green-700 font-bold">
                            {user.reward_balance.toFixed(5)}
                          </span>
                          <span className="text-green-600 text-xs">pts</span>
                        </div>
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          </div>
        )}
      </main>
    </div>
  );
}
